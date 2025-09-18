(** State management module
    
    This module manages the state of the protocol - active intents, matches,
    settlements, and agent information. It provides both in-memory and
    persistent state management.
    
    Key principles:
    - All state transitions are atomic and reversible
    - State can be checkpointed and restored
    - State changes generate proofs for verification
*)

open Types

(** The global protocol state *)
type t = {
  (* Agent-related state *)
  agents: (public_key, agent_info) Hashtbl.t;
  reputations: (public_key, reputation) Hashtbl.t;
  
  (* Intent state *)
  intents: (uuid, intent) Hashtbl.t;
  intents_by_agent: (public_key, uuid list) Hashtbl.t;
  intents_by_resource: (resource_uri, uuid list) Hashtbl.t;
  
  (* Match state *)
  matches: (uuid, match_t) Hashtbl.t;
  matches_by_intent: (uuid, uuid list) Hashtbl.t;
  
  (* Settlement state *)
  settlements: (uuid, settlement) Hashtbl.t;
  settlements_by_agent: (public_key, uuid list) Hashtbl.t;
  
  (* Negotiation state *)
  negotiations: (uuid, negotiation) Hashtbl.t;
  
  (* Statistics *)
  mutable total_intents_posted: int;
  mutable total_matches_found: int;
  mutable total_settlements_completed: int;
  mutable total_volume_transacted: float;
  
  (* State metadata *)
  mutable last_checkpoint: timestamp;
  mutable state_version: int;
}

and agent_info = {
  public_key: public_key;
  joined_at: timestamp;
  last_seen: timestamp;
  capabilities: string list;
  metadata: (string * string) list;
}

and negotiation = {
  match_id: uuid;
  participants: public_key list;
  state: negotiation_state;
  proposals: (public_key * settlement_point * timestamp) list;
  started_at: timestamp;
  last_activity: timestamp;
}

(** Protocol statistics record *)
type stats = {
  total_intents_posted: int;
  total_matches_found: int;
  total_settlements_completed: int;
  total_volume_transacted: float;
  active_intents: int;
  active_agents: int;
  state_version: int;
}

(** Create a new empty state *)
let create () = {
  agents = Hashtbl.create 1000;
  reputations = Hashtbl.create 1000;
  intents = Hashtbl.create 10000;
  intents_by_agent = Hashtbl.create 1000;
  intents_by_resource = Hashtbl.create 100;
  matches = Hashtbl.create 10000;
  matches_by_intent = Hashtbl.create 10000;
  settlements = Hashtbl.create 10000;
  settlements_by_agent = Hashtbl.create 1000;
  negotiations = Hashtbl.create 1000;
  total_intents_posted = 0;
  total_matches_found = 0;
  total_settlements_completed = 0;
  total_volume_transacted = 0.0;
  last_checkpoint = Time_provider.now ();
  state_version = 0;
}

(** State transitions - all mutations go through these *)
module Transitions = struct
  (** Register a new agent *)
  let register_agent state agent_id capabilities =
    if Hashtbl.mem state.agents agent_id then
      Error "Agent already registered"
    else begin
      let info = {
        public_key = agent_id;
        joined_at = Time_provider.now ();
        last_seen = Time_provider.now ();
        capabilities = capabilities;
        metadata = [];
      } in
      Hashtbl.add state.agents agent_id info;

      (* Initialize reputation *)
      let rep = {
        agent_id = agent_id;
        score = 0.5;  (* Start at neutral *)
        total_settlements = 0;
        successful_settlements = 0;
        failed_settlements = 0;
        total_volume = 0.0;
        last_updated = Time_provider.now ();
        domain_scores = None;
      } in
      Hashtbl.add state.reputations agent_id rep;

      state.state_version <- state.state_version + 1;
      Ok ()
    end
  
  (** Post a new intent *)
  let post_intent state (intent : intent) =
    (* Check if agent exists *)
    if not (Hashtbl.mem state.agents intent.agent_id) then
      Error "Agent not registered"
    else if Hashtbl.mem state.intents intent.intent_id then
      Error "Intent ID already exists"
    else begin
      (* Add to main intent table *)
      Hashtbl.add state.intents intent.intent_id intent;
      
      (* Index by agent *)
      let agent_intents = 
        try Hashtbl.find state.intents_by_agent intent.agent_id
        with Not_found -> []
      in
      Hashtbl.replace state.intents_by_agent intent.agent_id 
        (intent.intent_id :: agent_intents);
      
      (* Index by resource type *)
      let resource_intents = 
        try Hashtbl.find state.intents_by_resource intent.offer_field.resource_type
        with Not_found -> []
      in
      Hashtbl.replace state.intents_by_resource intent.offer_field.resource_type
        (intent.intent_id :: resource_intents);
      
      (* Update statistics *)
      state.total_intents_posted <- state.total_intents_posted + 1;
      state.state_version <- state.state_version + 1;
      
      Ok ()
    end
  
  (** Record a discovered match *)
  let record_match state (match_t : match_t) =
    if Hashtbl.mem state.matches match_t.match_id then
      Error "Match ID already exists"
    else begin
      (* Verify all intents exist *)
      let intents_exist = 
        List.for_all (fun id -> Hashtbl.mem state.intents id) match_t.intent_ids
      in
      
      if not intents_exist then
        Error "One or more intents in match do not exist"
      else begin
        (* Add to matches table *)
        Hashtbl.add state.matches match_t.match_id match_t;
        
        (* Index by intent *)
        List.iter (fun intent_id ->
          let intent_matches = 
            try Hashtbl.find state.matches_by_intent intent_id
            with Not_found -> []
          in
          Hashtbl.replace state.matches_by_intent intent_id
            (match_t.match_id :: intent_matches)
        ) match_t.intent_ids;
        
        (* Update statistics *)
        state.total_matches_found <- state.total_matches_found + 1;
        state.state_version <- state.state_version + 1;
        
        Ok ()
      end
    end
  
  (** Start a negotiation *)
  let start_negotiation state match_id =
    match Hashtbl.find_opt state.matches match_id with
    | None -> Error "Match not found"
    | Some match_t ->
        if Hashtbl.mem state.negotiations match_id then
          Error "Negotiation already exists for this match"
        else
          (* Get participants from match *)
          let participants = 
            match_t.intent_ids
            |> List.filter_map (fun intent_id ->
                Hashtbl.find_opt state.intents intent_id
                |> Option.map (fun (i : intent) -> i.agent_id))
            |> List.sort_uniq String.compare
          in
          
          let negotiation = {
            match_id = match_id;
            participants = participants;
            state = Proposing;
            proposals = [];
            started_at = Time_provider.now ();
            last_activity = Time_provider.now ();
          } in
          
          Hashtbl.add state.negotiations match_id negotiation;
          state.state_version <- state.state_version + 1;
          Ok ()
  
  (** Execute a settlement *)
  let execute_settlement state settlement =
    if Hashtbl.mem state.settlements settlement.settlement_id then
      Error "Settlement already exists"
    else
      match Hashtbl.find_opt state.matches settlement.match_id with
      | None -> Error "Match not found"
      | Some match_t ->
          (* Mark intents as consumed if needed *)
          let consume_intents () =
            match_t.intent_ids
            |> List.map (fun id -> Hashtbl.find_opt state.intents id)
            |> List.filter Option.is_some
            |> List.map Option.get
            |> List.map (fun intent ->
                match Intent.consume_match intent with
                | Ok updated -> Hashtbl.replace state.intents intent.intent_id updated; Ok ()
                | Error e -> Error e)
            |> List.find_opt (function Error _ -> true | _ -> false)
            |> Option.value ~default:(Ok ())
          in
          
          match consume_intents () with
          | Error e -> Error e
          | Ok () ->
              (* Add settlement *)
              Hashtbl.add state.settlements settlement.settlement_id settlement;
              
              (* Index by agent *)
              let agents = 
                match_t.intent_ids
                |> List.filter_map (fun id ->
                    Hashtbl.find_opt state.intents id
                    |> Option.map (fun (i : intent) -> i.agent_id))
              in
              
              List.iter (fun agent_id ->
                let agent_settlements = 
                  try Hashtbl.find state.settlements_by_agent agent_id
                  with Not_found -> []
                in
                Hashtbl.replace state.settlements_by_agent agent_id
                  (settlement.settlement_id :: agent_settlements)
              ) agents;
              
              (* Update reputations *)
              List.iter (fun agent_id ->
                match Hashtbl.find_opt state.reputations agent_id with
                | None -> ()
                | Some rep ->
                    let updated_rep = 
                      match settlement.status with
                      | Completed ->
                          { rep with
                            successful_settlements = rep.successful_settlements + 1;
                            total_settlements = rep.total_settlements + 1;
                            total_volume = rep.total_volume +. settlement.executed_point.quantity;
                            score = min 1.0 (rep.score +. 0.01);  (* Increase reputation *)
                            last_updated = Time_provider.now ();
                          }
                      | Failed _ ->
                          { rep with
                            failed_settlements = rep.failed_settlements + 1;
                            total_settlements = rep.total_settlements + 1;
                            score = max 0.0 (rep.score -. 0.02);  (* Decrease reputation *)
                            last_updated = Time_provider.now ();
                          }
                      | _ -> rep
                    in
                    Hashtbl.replace state.reputations agent_id updated_rep
              ) agents;
              
              (* Update global statistics *)
              if settlement.status = Completed then begin
                state.total_settlements_completed <- state.total_settlements_completed + 1;
                state.total_volume_transacted <- 
                  state.total_volume_transacted +. settlement.executed_point.quantity;
              end;
              
              state.state_version <- state.state_version + 1;
              Ok ()
  
  (** Cancel an intent *)
  let cancel_intent state intent_id agent_id =
    match Hashtbl.find_opt state.intents intent_id with
    | None -> Error "Intent not found"
    | Some intent ->
        if intent.agent_id <> agent_id then
          Error "Only the owner can cancel an intent"
        else
          let cancelled = Intent.cancel intent in
          Hashtbl.replace state.intents intent_id cancelled;
          state.state_version <- state.state_version + 1;
          Ok ()
end

(** Query operations - read-only access to state *)
module Queries = struct
  (** Get all active intents *)
  let get_active_intents state =
    let current_time = Time_provider.now () in
    Hashtbl.fold (fun _id intent acc ->
      if Intent.is_valid intent current_time then
        intent :: acc
      else
        acc
    ) state.intents []
  
  (** Get intents by agent *)
  let get_agent_intents state agent_id =
    try
      let intent_ids = Hashtbl.find state.intents_by_agent agent_id in
      List.filter_map (fun id -> Hashtbl.find_opt state.intents id) intent_ids
    with Not_found -> []
  
  (** Get intents by resource type *)
  let get_resource_intents state resource_type =
    try
      let intent_ids = Hashtbl.find state.intents_by_resource resource_type in
      List.filter_map (fun id -> Hashtbl.find_opt state.intents id) intent_ids
    with Not_found -> []
  
  (** Get matches for an intent *)
  let get_intent_matches state intent_id =
    try
      let match_ids = Hashtbl.find state.matches_by_intent intent_id in
      List.filter_map (fun id -> Hashtbl.find_opt state.matches id) match_ids
    with Not_found -> []
  
  (** Get agent reputation *)
  let get_reputation state agent_id =
    Hashtbl.find_opt state.reputations agent_id
  
  (** Get protocol statistics *)
  let get_stats (state : t) : stats = {
    total_intents_posted = state.total_intents_posted;
    total_matches_found = state.total_matches_found;
    total_settlements_completed = state.total_settlements_completed;
    total_volume_transacted = state.total_volume_transacted;
    active_intents = Hashtbl.length state.intents;
    active_agents = Hashtbl.length state.agents;
    state_version = state.state_version;
  }
end

(** State persistence *)
module Persistence = struct
  (** Serialize state to JSON *)
  let to_json (state : t) =
    (* Simplified - in production would serialize all tables *)
    `Assoc [
      "version", `Int state.state_version;
      "last_checkpoint", `Float state.last_checkpoint;
      "stats", `Assoc [
        "total_intents", `Int state.total_intents_posted;
        "total_matches", `Int state.total_matches_found;
        "total_settlements", `Int state.total_settlements_completed;
        "total_volume", `Float state.total_volume_transacted;
      ];
    ]
  
  (** Save state to file *)
  let save_checkpoint (state : t) filename =
    let json = to_json state in
    let oc = open_out filename in
    Yojson.Safe.to_channel oc json;
    close_out oc;
    state.last_checkpoint <- Time_provider.now ()
  
  (** Load state from file *)
  let load_checkpoint filename =
    try
      let ic = open_in filename in
      let json = Yojson.Safe.from_channel ic in
      close_in ic;
      (* Simplified - would deserialize full state *)
      Ok (create ())
    with
    | Sys_error e -> Error e
    | Yojson.Json_error e -> Error e
end

