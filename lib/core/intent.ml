(** Intent management module
    
    This module handles the creation, validation, and manipulation of intents.
    Intents are the core primitive of ambient commerce - they represent
    persistent economic desires that exist in the network until matched.
    
    Key design principles:
    - Intents are immutable once created (only lifecycle evolves)
    - All intent operations are deterministic for verification
    - Intents can be validated without network access
*)

open Types

(** Generate a new UUID. In production, use a proper UUID library. *)
let generate_uuid () = 
  (* For now, using timestamp + random for uniqueness *)
  Printf.sprintf "%f-%d" (Unix.time ()) (Random.int 1000000)

(** Get current timestamp *)
let now () = Unix.time ()

(** Calculate the cryptographic commitment for an intent.
    This ensures the intent cannot be modified after creation. *)
let calculate_commitment intent = 
  (* In production, this would be a proper cryptographic hash (SHA256, etc.)
     For now, we'll create a simple string representation *)
  let content = 
    Printf.sprintf "%s:%s:%s:%s:%f"
      intent.intent_id
      intent.agent_id
      (resource_field_to_yojson intent.offer_field |> Yojson.Safe.to_string)
      (resource_field_to_yojson intent.want_field |> Yojson.Safe.to_string)
      intent.created_at
  in
  (* In production: Sha256.string content |> Sha256.to_hex *)
  "HASH:" ^ content

(** Validate a resource field.
    Ensures the field makes semantic sense. *)
let validate_resource_field field =
  let open Result in
  (* Check quantity range is valid *)
  let (min_qty, max_qty) = field.quantity_range in
  if min_qty < 0.0 then
    Error "Minimum quantity cannot be negative"
  else if max_qty < min_qty then
    Error "Maximum quantity must be >= minimum quantity"
  else if min_qty = 0.0 && max_qty = 0.0 then
    Error "Quantity range cannot be [0,0]"
  else
    (* Check quality constraints make sense *)
    match field.quality with
    | Graded score when score < 0.0 || score > 1.0 ->
        Error "Quality score must be between 0.0 and 1.0"
    | _ -> Ok ()

(** Validate constraints for internal consistency *)
let validate_constraints constraints =
  let open Result in
  let rec check = function
    | [] -> Ok ()
    | TimeWindow (start, end_) :: rest ->
        if start >= end_ then
          Error "Time window start must be before end"
        else
          check rest
    | PriceRange (min_price, max_price) :: rest ->
        if min_price < 0.0 then
          Error "Minimum price cannot be negative"
        else if max_price < min_price then
          Error "Maximum price must be >= minimum price"
        else
          check rest
    | Counterparty req :: rest ->
        (match req.min_reputation with
         | Some rep when rep < 0.0 || rep > 1.0 ->
             Error "Reputation must be between 0.0 and 1.0"
         | _ -> check rest)
    | Custom _ :: rest -> check rest
  in
  check constraints

(** Create a new intent with validation *)
let create 
    ~agent_id 
    ~offers 
    ~wants 
    ?(constraints = [])
    ?(lifecycle = Expiring (now () +. 86400.0))  (* Default 24 hours *)
    ?(priority = 1.0)
    () =

  (* Validate inputs *)
  match validate_resource_field offers with
  | Error e -> Error e
  | Ok () ->
  match validate_resource_field wants with
  | Error e -> Error e
  | Ok () ->
  match validate_constraints constraints with
  | Error e -> Error e
  | Ok () ->

  (* Check priority is valid *)
  if priority < 0.0 then
    Error "Priority cannot be negative"
  else

  (* Create the intent *)
  let intent = {
    intent_id = generate_uuid ();
    agent_id = agent_id;
    offer_field = offers;
    want_field = wants;
    constraints = constraints;
    lifecycle = lifecycle;
    created_at = now ();
    updated_at = now ();
    commitment = "";  (* Will be set below *)
    priority = priority;
  } in
  
  (* Calculate and set commitment *)
  let intent = { intent with commitment = calculate_commitment intent } in
  
  Ok intent

(** Check if an intent is still valid at a given time *)
let is_valid intent current_time =
  match intent.lifecycle with
  | Eternal -> true
  | Expiring expiry -> current_time < expiry
  | Consumable remaining -> remaining > 0
  | Conditional _condition ->
      (* Conditional validity requires external evaluation *)
      (* For now, assume true - in practice, would query oracle *)
      true

(** Check if an intent's constraints are satisfied at a given time *)
let constraints_satisfied intent current_time =
  let check_constraint = function
    | TimeWindow (start, end_) ->
        current_time >= start && current_time <= end_
    | PriceRange _ -> 
        (* Price constraints are checked during matching *)
        true
    | Counterparty _ ->
        (* Counterparty constraints are checked during matching *)
        true
    | Custom _ ->
        (* Custom constraints require external evaluation *)
        true
  in
  List.for_all check_constraint intent.constraints

(** Check if two intents are potentially compatible.
    This is a quick check before detailed matching. *)
let are_compatible intent_a intent_b =
  (* Check if A offers what B wants and vice versa *)
  let resources_match = 
    intent_a.offer_field.resource_type = intent_b.want_field.resource_type &&
    intent_b.offer_field.resource_type = intent_a.want_field.resource_type
  in
  
  (* Check if quantity ranges overlap *)
  let quantities_overlap =
    let (a_min, a_max) = intent_a.offer_field.quantity_range in
    let (b_min, b_max) = intent_b.want_field.quantity_range in
    a_max >= b_min && b_max >= a_min
  in
  
  (* Check if qualities are compatible *)
  let qualities_compatible =
    match intent_a.offer_field.quality, intent_b.want_field.quality with
    | Fungible, Fungible -> true
    | Graded a, Graded b -> a >= b  (* Offered quality meets requirement *)
    | Unique id1, Unique id2 -> id1 = id2
    | Fungible, Graded _ -> true  (* Fungible can satisfy any quality requirement *)
    | _ -> false
  in
  
  resources_match && quantities_overlap && qualities_compatible

(** Calculate the priority score for matching.
    Higher scores match first. *)
let priority_score intent =
  (* Base priority from the intent *)
  let base = intent.priority in
  
  (* Age factor - older intents get slight boost to prevent starvation *)
  let age = now () -. intent.created_at in
  let age_boost = min 0.1 (age /. 3600.0 *. 0.01) in  (* 0.01 per hour, max 0.1 *)
  
  base +. age_boost

(** Extract time windows from constraints *)
let get_time_windows intent =
  intent.constraints
  |> List.filter_map (function
      | TimeWindow (start, end_) -> Some (start, end_)
      | _ -> None)

(** Extract price ranges from constraints *)
let get_price_ranges intent =
  intent.constraints
  |> List.filter_map (function
      | PriceRange (min_p, max_p) -> Some (min_p, max_p)
      | _ -> None)

(** Extract counterparty requirements *)
let get_counterparty_requirements intent =
  intent.constraints
  |> List.filter_map (function
      | Counterparty req -> Some req
      | _ -> None)

(** Check if an agent meets counterparty requirements *)
let meets_counterparty_requirements intent counterparty_id counterparty_reputation =
  let requirements = get_counterparty_requirements intent in
  List.for_all (fun req ->
    (* Check reputation if required *)
    let rep_ok = match req.min_reputation with
      | None -> true
      | Some min_rep -> counterparty_reputation >= min_rep
    in
    
    (* Check blacklist *)
    let not_excluded = not (List.mem counterparty_id req.excluded_agents) in
    
    (* Check whitelist if present *)
    let is_preferred = 
      match req.preferred_agents with
      | [] -> true  (* No preference *)
      | preferred -> List.mem counterparty_id preferred
    in
    
    rep_ok && not_excluded && is_preferred
  ) requirements

(** Consume one match from a consumable intent *)
let consume_match intent =
  match intent.lifecycle with
  | Consumable n when n > 0 ->
      Ok { intent with 
           lifecycle = Consumable (n - 1);
           updated_at = now () }
  | Consumable _ ->
      Error "Intent already fully consumed"
  | _ ->
      Ok intent  (* Non-consumable intents unchanged *)

(** Cancel an intent (mark as expired) *)
let cancel intent =
  { intent with 
    lifecycle = Expiring (now ());
    updated_at = now () }

(** Extend the expiry of an intent *)
let extend_expiry intent additional_time =
  match intent.lifecycle with
  | Expiring current_expiry ->
      Ok { intent with 
           lifecycle = Expiring (current_expiry +. additional_time);
           updated_at = now () }
  | _ ->
      Error "Can only extend expiry of time-limited intents"

(** Compare intents for sorting (by priority) *)
let compare_by_priority intent_a intent_b =
  Float.compare 
    (priority_score intent_b)  (* Reversed for descending order *)
    (priority_score intent_a)

(** Convert intent to a simplified string representation for debugging *)
let to_string intent =
  Printf.sprintf "Intent[%s]: %s offers %s for %s (priority: %.2f)"
    intent.intent_id
    intent.agent_id
    intent.offer_field.resource_type
    intent.want_field.resource_type
    intent.priority

(** Statistics about an intent *)
type intent_stats = {
  age: float;                 (* Seconds since creation *)
  time_remaining: float option;  (* Seconds until expiry, if applicable *)
  matches_remaining: int option; (* For consumable intents *)
  is_active: bool;
}

(** Get statistics about an intent *)
let get_stats intent =
  let current = now () in
  let age = current -. intent.created_at in
  
  let (time_remaining, matches_remaining) = 
    match intent.lifecycle with
    | Eternal -> (None, None)
    | Expiring expiry -> 
        (Some (max 0.0 (expiry -. current)), None)
    | Consumable n -> 
        (None, Some n)
    | Conditional _ -> 
        (None, None)
  in
  
  {
    age = age;
    time_remaining = time_remaining;
    matches_remaining = matches_remaining;
    is_active = is_valid intent current;
  }

(** Batch operations for efficiency *)
module Batch = struct
  (** Filter a list of intents to only valid ones *)
  let filter_valid intents current_time =
    List.filter (fun i -> is_valid i current_time) intents
  
  (** Sort intents by priority *)
  let sort_by_priority intents =
    List.sort compare_by_priority intents
  
  (** Find all compatible intent pairs in a list *)
  let find_compatible_pairs intents =
    let rec pairs acc = function
      | [] -> acc
      | h :: t ->
          let compatible_with_h = 
            List.filter (are_compatible h) t
            |> List.map (fun i -> (h, i))
          in
          pairs (compatible_with_h @ acc) t
    in
    pairs [] intents
  
  (** Group intents by resource type for efficient matching *)
  let group_by_resource intents =
    let table = Hashtbl.create 100 in
    List.iter (fun intent ->
      let key = intent.offer_field.resource_type in
      let current = 
        try Hashtbl.find table key
        with Not_found -> []
      in
      Hashtbl.replace table key (intent :: current)
    ) intents;
    table
end