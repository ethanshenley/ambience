(** Dynamic Resource Registry Implementation *)

open Types

(** Resource metadata *)
type resource_metadata = {
  uri: string;
  discovered_at: timestamp;
  first_seen_from: public_key;
  description: string option;
  properties: (string * string) list;
  parent: string option;
  mutable total_usage: int;
  mutable successful_trades: int;
  mutable failed_trades: int;
}

(** Endorsement of a resource type *)
type endorsement = {
  agent_id: public_key;
  reputation_stake: float;
  timestamp: timestamp;
  notes: string option;
  mutable outcome: endorsement_outcome option;
}

and endorsement_outcome =
  | Validated
  | Invalidated
  | Pending

(** Usage statistics *)
type usage_stats = {
  first_seen: timestamp;
  last_seen: timestamp;
  total_intents: int;
  total_matches: int;
  total_settlements: int;
  success_rate: float;
  avg_reputation_users: float;
}

(** Registry type *)
type t = {
  resources: (string, resource_metadata) Hashtbl.t;
  endorsements: (string, endorsement list) Hashtbl.t;
  usage_counts: (string, int * int * int) Hashtbl.t;  (* intents, matches, settlements *)
  mappings: (string, string) Hashtbl.t;               (* local -> canonical *)
  mutable event_log: registry_event list;
  mutable last_update: timestamp;
}

(** Registry events *)
type registry_event =
  | ResourceDiscovered of string * public_key * timestamp
  | ResourceEndorsed of string * endorsement
  | ResourceUsed of string * string * bool
  | ResourceMapped of string * string

(** Create a new registry *)
let create () = {
  resources = Hashtbl.create 1000;
  endorsements = Hashtbl.create 1000;
  usage_counts = Hashtbl.create 1000;
  mappings = Hashtbl.create 100;
  event_log = [];
  last_update = Time_provider.now ();
}

(** Clear all registry entries *)
let clear t =
  Hashtbl.clear t.resources;
  Hashtbl.clear t.endorsements;
  Hashtbl.clear t.usage_counts;
  Hashtbl.clear t.mappings;
  t.event_log <- [];
  t.last_update <- Time_provider.now ()

(** {1 Resource Discovery} *)

let discover t ~uri ~agent_id ?description ?properties ?parent () =
  if not (Hashtbl.mem t.resources uri) then
    let metadata = {
      uri;
      discovered_at = Time_provider.now ();
      first_seen_from = agent_id;
      description;
      properties = Option.value properties ~default:[];
      parent;
      total_usage = 0;
      successful_trades = 0;
      failed_trades = 0;
    } in
    Hashtbl.add t.resources uri metadata;
    t.event_log <- ResourceDiscovered (uri, agent_id, metadata.discovered_at) :: t.event_log;
    t.last_update <- Time_provider.now ()

let exists t uri =
  Hashtbl.mem t.resources uri

let lookup t uri =
  Hashtbl.find_opt t.resources uri

let search t ~pattern =
  Hashtbl.fold (fun uri metadata acc ->
    if String.contains uri pattern.[0] ||
       (match metadata.description with
        | Some desc -> String.contains desc pattern.[0]
        | None -> false)
    then metadata :: acc
    else acc
  ) t.resources []

let list_all t =
  Hashtbl.fold (fun _ metadata acc -> metadata :: acc) t.resources []

(** {1 Endorsement System} *)

let endorse t ~uri ~agent_id ~stake ?notes () =
  if stake < 0.1 || stake > 100.0 then
    Error "Stake must be between 0.1 and 100.0"
  else if not (exists t uri) then
    Error (Printf.sprintf "Resource %s not found" uri)
  else
    let endorsement = {
      agent_id;
      reputation_stake = stake;
      timestamp = Time_provider.now ();
      notes;
      outcome = Some Pending;
    } in
    let current = Hashtbl.find_opt t.endorsements uri |> Option.value ~default:[] in

    (* Check if agent already endorsed *)
    if List.exists (fun e -> e.agent_id = agent_id) current then
      Error "Agent has already endorsed this resource"
    else begin
      Hashtbl.replace t.endorsements uri (endorsement :: current);
      t.event_log <- ResourceEndorsed (uri, endorsement) :: t.event_log;
      t.last_update <- Time_provider.now ();
      Ok ()
    end

let get_endorsements t uri =
  Hashtbl.find_opt t.endorsements uri |> Option.value ~default:[]

let get_endorsement_score t uri =
  let endorsements = get_endorsements t uri in
  if endorsements = [] then 0.0
  else
    let total_stake = List.fold_left (fun acc e ->
      match e.outcome with
      | Some Invalidated -> acc  (* Don't count failed endorsements *)
      | _ -> acc +. e.reputation_stake
    ) 0.0 endorsements in

    (* Normalize to 0-1 scale, with diminishing returns *)
    min 1.0 (total_stake /. 100.0)

let update_endorsement_outcome t ~uri ~agent_id ~outcome =
  match Hashtbl.find_opt t.endorsements uri with
  | None -> ()
  | Some endorsements ->
      List.iter (fun e ->
        if e.agent_id = agent_id then
          e.outcome <- Some outcome
      ) endorsements

(** {1 Usage Tracking} *)

let record_usage t ~uri ~usage_type ~success =
  (* Update metadata *)
  (match lookup t uri with
   | Some metadata ->
       metadata.total_usage <- metadata.total_usage + 1;
       if success then
         metadata.successful_trades <- metadata.successful_trades + 1
       else
         metadata.failed_trades <- metadata.failed_trades + 1
   | None -> ());

  (* Update usage counts *)
  let (intents, matches, settlements) =
    Hashtbl.find_opt t.usage_counts uri |> Option.value ~default:(0, 0, 0) in

  let updated = match usage_type with
    | `Intent -> (intents + 1, matches, settlements)
    | `Match -> (intents, matches + 1, settlements)
    | `Settlement -> (intents, matches, settlements + 1)
  in
  Hashtbl.replace t.usage_counts uri updated;
  t.last_update <- Time_provider.now ()

let get_usage_stats t uri =
  match lookup t uri, Hashtbl.find_opt t.usage_counts uri with
  | Some metadata, Some (intents, matches, settlements) ->
      let total_trades = metadata.successful_trades + metadata.failed_trades in
      Some {
        first_seen = metadata.discovered_at;
        last_seen = Time_provider.now ();  (* Should track this properly *)
        total_intents = intents;
        total_matches = matches;
        total_settlements = settlements;
        success_rate = if total_trades > 0 then
          float_of_int metadata.successful_trades /. float_of_int total_trades
        else 0.0;
        avg_reputation_users = 0.7;  (* Placeholder - should track actual users *)
      }
  | _ -> None

let get_credibility_score t uri =
  let endorsement_score = get_endorsement_score t uri in
  let usage_score =
    match get_usage_stats t uri with
    | None -> 0.0
    | Some stats ->
        (* More usage = more credible *)
        let usage_factor = min 1.0 (float_of_int stats.total_intents /. 100.0) in
        (* Higher success rate = more credible *)
        let success_factor = stats.success_rate in
        (usage_factor +. success_factor) /. 2.0
  in
  (* Weighted average: endorsements matter more initially, usage matters more over time *)
  (endorsement_score *. 0.6 +. usage_score *. 0.4)

(** {1 Resource Relationships} *)

let suggest_parent t ~uri ~parent ~agent_id =
  match lookup t uri with
  | None -> ()
  | Some metadata ->
      (* For now, just accept the first suggested parent *)
      if metadata.parent = None then begin
        let updated = { metadata with parent = Some parent } in
        Hashtbl.replace t.resources uri updated
      end

let get_children t parent_uri =
  Hashtbl.fold (fun uri metadata acc ->
    match metadata.parent with
    | Some p when p = parent_uri -> uri :: acc
    | _ -> acc
  ) t.resources []

let rec get_hierarchy t uri =
  match lookup t uri with
  | None -> []
  | Some metadata ->
      (uri, 0) :: (match metadata.parent with
        | None -> []
        | Some parent ->
            List.map (fun (u, d) -> (u, d + 1)) (get_hierarchy t parent))

let find_similar t uri ~limit =
  match lookup t uri with
  | None -> []
  | Some target ->
      let scores = Hashtbl.fold (fun other_uri other acc ->
        if other_uri = uri then acc
        else
          let score =
            (* Same parent = high similarity *)
            (if target.parent = other.parent && target.parent <> None then 0.5 else 0.0) +.
            (* Shared properties = medium similarity *)
            (let shared = List.filter (fun (k1, v1) ->
               List.exists (fun (k2, v2) -> k1 = k2 && v1 = v2) other.properties
             ) target.properties in
             float_of_int (List.length shared) *. 0.1) +.
            (* Similar URI structure = low similarity *)
            (if String.contains uri ':' && String.contains other_uri ':' then
               let parts1 = String.split_on_char ':' uri in
               let parts2 = String.split_on_char ':' other_uri in
               if List.hd parts1 = List.hd parts2 then 0.2 else 0.0
             else 0.0)
          in
          if score > 0.0 then (other_uri, score) :: acc else acc
      ) t.resources []
      in
      scores
      |> List.sort (fun (_, s1) (_, s2) -> Float.compare s2 s1)
      |> List.filteri (fun i _ -> i < limit)

(** {1 Analytics} *)

let trending t ?window ~limit =
  let cutoff = match window with
    | None -> 0.0  (* All time *)
    | Some w -> Time_provider.now () -. w
  in

  (* Count recent usages *)
  let scores = Hashtbl.fold (fun uri metadata acc ->
    if metadata.discovered_at >= cutoff then
      let score =
        float_of_int metadata.total_usage +.
        get_endorsement_score t uri *. 10.0 +.
        (if metadata.successful_trades > 0 then 5.0 else 0.0)
      in
      (uri, score) :: acc
    else acc
  ) t.resources []
  |> List.sort (fun (_, s1) (_, s2) -> Float.compare s2 s1)
  |> List.filteri (fun i _ -> i < limit)
  in
  scores

let get_innovation_rate t =
  let day_ago = Time_provider.now () -. 86400.0 in
  let recent_count = Hashtbl.fold (fun _ metadata acc ->
    if metadata.discovered_at >= day_ago then acc + 1 else acc
  ) t.resources 0 in
  float_of_int recent_count

let get_endorsement_accuracy t agent_id =
  let outcomes = Hashtbl.fold (fun _ endorsements acc ->
    List.fold_left (fun acc e ->
      if e.agent_id = agent_id then
        match e.outcome with
        | Some Validated -> (1, 0) :: acc
        | Some Invalidated -> (0, 1) :: acc
        | _ -> acc
      else acc
    ) acc endorsements
  ) t.endorsements [] in

  let (validated, invalidated) = List.fold_left (fun (v, i) (vx, ix) ->
    (v + vx, i + ix)
  ) (0, 0) outcomes in

  if validated + invalidated = 0 then 1.0  (* No data = assume good *)
  else float_of_int validated /. float_of_int (validated + invalidated)

(** {1 Synchronization} *)

type registry_update = {
  update_type: [`Discovery | `Endorsement | `Usage];
  resource_uri: string;
  data: string;
  timestamp: timestamp;
  source: public_key;
}

let get_updates_since t since =
  List.filter_map (fun event ->
    match event with
    | ResourceDiscovered (uri, agent, ts) when ts > since ->
        Some {
          update_type = `Discovery;
          resource_uri = uri;
          data = Printf.sprintf "{\"agent\":\"%s\"}" agent;
          timestamp = ts;
          source = agent;
        }
    | ResourceEndorsed (uri, endorsement) when endorsement.timestamp > since ->
        Some {
          update_type = `Endorsement;
          resource_uri = uri;
          data = Printf.sprintf "{\"stake\":%.2f}" endorsement.reputation_stake;
          timestamp = endorsement.timestamp;
          source = endorsement.agent_id;
        }
    | _ -> None
  ) t.event_log

let apply_update t update =
  match update.update_type with
  | `Discovery ->
      discover t ~uri:update.resource_uri ~agent_id:update.source ()
  | `Endorsement ->
      (* Parse stake from JSON - simplified *)
      let stake = 1.0 in  (* Should parse from update.data *)
      let _ = endorse t ~uri:update.resource_uri ~agent_id:update.source ~stake () in
      ()
  | `Usage ->
      record_usage t ~uri:update.resource_uri ~usage_type:`Intent ~success:true

let merge t other =
  (* Merge resources *)
  Hashtbl.iter (fun uri metadata ->
    if not (exists t uri) then
      Hashtbl.add t.resources uri metadata
  ) other.resources;

  (* Merge endorsements *)
  Hashtbl.iter (fun uri endorsements ->
    let current = get_endorsements t uri in
    let merged = List.append current endorsements
      |> List.sort_uniq (fun e1 e2 -> String.compare e1.agent_id e2.agent_id) in
    Hashtbl.replace t.endorsements uri merged
  ) other.endorsements

(** {1 Persistence} *)

let to_json t =
  let resources_json = Hashtbl.fold (fun uri metadata acc ->
    `Assoc [
      ("uri", `String uri);
      ("discovered_at", `Float metadata.discovered_at);
      ("first_seen_from", `String metadata.first_seen_from);
      ("description", match metadata.description with
        | None -> `Null
        | Some d -> `String d);
      ("total_usage", `Int metadata.total_usage);
    ] :: acc
  ) t.resources [] in

  `Assoc [
    ("resources", `List resources_json);
    ("last_update", `Float t.last_update);
  ]

let from_json json =
  let t = create () in
  (* Simplified - should properly parse JSON *)
  t

(** {1 Seeding and Defaults} *)

let seed_common_resources t =
  (* Add commonly used resources with high initial endorsements *)
  let common = [
    ("currency:fiat:USD", "US Dollar", ["type", "fiat"; "symbol", "$"]);
    ("currency:crypto:BTC", "Bitcoin", ["type", "crypto"; "network", "bitcoin"]);
    ("currency:crypto:ETH", "Ethereum", ["type", "crypto"; "network", "ethereum"]);
    ("compute:gpu:hours", "GPU Compute Hours", ["type", "compute"; "unit", "hours"]);
    ("storage:gb", "Gigabytes Storage", ["type", "storage"; "unit", "GB"]);
    ("energy:kwh", "Kilowatt Hours", ["type", "energy"; "unit", "kWh"]);
    ("bandwidth:mbps", "Megabits per second", ["type", "network"; "unit", "Mbps"]);
    ("time:hours", "Human Hours", ["type", "service"; "unit", "hours"]);
  ] in

  List.iter (fun (uri, desc, props) ->
    discover t ~uri ~agent_id:"system" ~description:desc ~properties:props ();
    (* Give system endorsement *)
    let _ = endorse t ~uri ~agent_id:"system" ~stake:50.0
              ~notes:"Common resource type" () in
    ()
  ) common

let add_mapping t ~local ~canonical =
  Hashtbl.add t.mappings local canonical

let resolve t uri =
  Hashtbl.find_opt t.mappings uri |> Option.value ~default:uri