(** Ambient Commerce Protocol - Main Entry Point
    
    This module provides a high-level interface to the entire protocol,
    coordinating all layers and providing a simple API for applications.
*)

open Ambience_core
open Ambience_matching
open Ambience_network
open Ambience_settlement
open Ambience_trust

(** Protocol node configuration *)
type node_config = {
  (* Identity *)
  node_id: Types.public_key;
  node_name: string;
  
  (* Network *)
  listen_port: int;
  bootstrap_nodes: string list;
  max_peers: int;
  
  (* Matching *)
  matching_interval: float;
  enable_multilateral: bool;
  
  (* Settlement *)
  require_escrow: bool;
  reversal_window: float;
  
  (* Trust *)
  min_reputation: float;
  require_collateral: bool;
  collateral_rate: float;
}

(** Default configuration *)
let default_config node_id = {
  node_id = node_id;
  node_name = "ACP Node";
  listen_port = 8545;
  bootstrap_nodes = [];
  max_peers = 50;
  matching_interval = 1.0;
  enable_multilateral = false;
  require_escrow = true;
  reversal_window = 3600.0;
  min_reputation = 0.3;
  require_collateral = true;
  collateral_rate = 0.1;
}

(** Ambient protocol node *)
type node = {
  config: node_config;
  
  (* Core *)
  state: State.t;
  
  (* Matching *)
  matching_engine: Engine.t;
  discovery_context: Discovery.context;
  
  (* Network *)
  peer_manager: Peer.peer_manager;
  gossip_engine: Gossip.gossip_engine;
  
  (* Settlement *)
  escrow_manager: Escrow.escrow_manager;
  executor: Executor.executor;
  proof_generator: Proof.proof_generator;
  reversal_manager: Reversal.reversal_manager;
  
  (* Trust *)
  capability_manager: Capability.capability_manager;
  collateral_manager: Collateral.collateral_manager;
  reputation_manager: Reputation.reputation_manager;
  
  (* Status *)
  mutable running: bool;
  start_time: Types.timestamp;
}

(** Create a new protocol node *)
let create_node config =
  (* Initialize core *)
  let state = State.create () in
  
  (* Initialize matching *)
  let matching_config = {
    Engine.default_config with
    matching_interval = config.matching_interval;
    enable_multilateral = config.enable_multilateral;
  } in
  let matching_engine = Engine.create ~config:matching_config state in
  let discovery_context = Discovery.create_context () in
  
  (* Initialize network *)
  let peer_manager = Peer.create_manager config.node_id ~max_peers:config.max_peers () in
  let gossip_engine = Gossip.create_engine peer_manager config.node_id () in
  
  (* Initialize settlement *)
  let escrow_manager = Escrow.create_manager () in
  let proof_generator = Proof.create_generator () in
  let executor = Executor.create state escrow_manager 
    ~require_escrow:config.require_escrow () in
  let reversal_manager = Reversal.create_manager proof_generator 
    ~reversal_window:config.reversal_window () in
  
  (* Initialize trust *)
  let capability_manager = Capability.create_manager () in
  let collateral_manager = Collateral.create_manager 
    ~default_type:(Collateral.Percentage config.collateral_rate) () in
  let reputation_manager = Reputation.create_manager () in
  
  (* Register as agent in state *)
  let _ = State.Transitions.register_agent state config.node_id ["node"] in
  
  (* Initialize reputation *)
  let _ = Reputation.initialize_reputation reputation_manager config.node_id () in
  
  {
    config = config;
    state = state;
    matching_engine = matching_engine;
    discovery_context = discovery_context;
    peer_manager = peer_manager;
    gossip_engine = gossip_engine;
    escrow_manager = escrow_manager;
    executor = executor;
    proof_generator = proof_generator;
    reversal_manager = reversal_manager;
    capability_manager = capability_manager;
    collateral_manager = collateral_manager;
    reputation_manager = reputation_manager;
    running = false;
    start_time = Ambience_core.Time_provider.now ();
  }

(** {2 High-Level Operations} *)

(** Post an intent to the network *)
let post_intent node ~offers ~wants ?(constraints = []) ?(lifecycle = None) () =
  (* Check reputation *)
  let reputation = Reputation.get_reputation node.reputation_manager node.config.node_id in
  if reputation < node.config.min_reputation then
    Error "Insufficient reputation to post intent"
  else
    (* Check capabilities *)
    if not (Capability.can_trade node.capability_manager node.config.node_id 
              offers.Types.resource_type) then
      Error "Not authorized to trade this resource"
    else
      (* Create intent *)
      match Intent.create 
              ~agent_id:node.config.node_id
              ~offers:offers
              ~wants:wants
              ~constraints:constraints
              ?lifecycle:lifecycle
              () with
      | Error e -> Error e
      | Ok intent ->
          (* Post to local state *)
          match State.Transitions.post_intent node.state intent with
          | Error e -> Error e
          | Ok () ->
              (* Broadcast to network *)
              let msg = Protocol.create_intent_msg node.config.node_id intent in
              Gossip.broadcast node.gossip_engine msg;
              
              Ok intent.intent_id

(** Execute a discovered match *)
let execute_match node match_t =
  (* Check if we're involved *)
  let our_intents : Types.intent list =
    match_t.Types.intent_ids
    |> List.filter_map (fun id ->
        State.Queries.get_active_intents node.state
        |> List.find_opt (fun (i : Types.intent) -> i.Types.intent_id = id))
    |> List.filter (fun (i : Types.intent) -> i.Types.agent_id = node.config.node_id)
  in
  
  if List.length our_intents = 0 then
    Error "Not a participant in this match"
  else
    (* Check collateral *)
    let reputation = Reputation.get_reputation node.reputation_manager node.config.node_id in
    let required_collateral =
      Collateral.calculate_required node.collateral_manager
        (List.hd match_t.settlement_space.pareto_frontier).quantity
        reputation
        (Collateral.Percentage 0.1)
    in
    
    if not (Collateral.has_sufficient_collateral node.collateral_manager 
              node.config.node_id required_collateral) then
      Error "Insufficient collateral"
    else
      (* Create settlement *)
      let settlement = {
        Types.settlement_id = Intent.generate_uuid ();
        match_id = match_t.match_id;
        executed_point = List.hd match_t.settlement_space.pareto_frontier;
        execution_proof = "";
        pre_state_hash = "";
        post_state_hash = "";
        reversible_until = Some (Ambience_core.Time_provider.now () +. node.config.reversal_window);
        status = Types.Pending;
        executed_at = Ambience_core.Time_provider.now ();
      } in
      
      (* Execute through executor *)
      Executor.execute node.executor settlement match_t

(** Query network for intents *)
let query_intents node ?(resource_type = None) ?(max_results = 100) () =
  let all_intents = State.Queries.get_active_intents node.state in
  
  let filtered = 
    match resource_type with
    | None -> all_intents
    | Some rtype ->
        List.filter (fun i ->
          i.Types.offer_field.resource_type = rtype ||
          i.Types.want_field.resource_type = rtype
        ) all_intents
  in
  
  List.filteri (fun i _ -> i < max_results) filtered

(** Get node statistics *)
let get_node_stats node =
  let state_stats = State.Queries.get_stats node.state in
  let matching_stats = Engine.get_stats node.matching_engine in
  let peer_stats = Peer.get_manager_stats node.peer_manager in
  let settlement_stats = Executor.get_stats node.executor in
  let reputation_stats = Reputation.get_stats node.reputation_manager in
  
  `Assoc [
    "node_id", `String node.config.node_id;
    "uptime", `Float (Ambience_core.Time_provider.now () -. node.start_time);
    "state", `Assoc [
      "total_intents", `Int state_stats.total_intents_posted;
      "active_intents", `Int state_stats.active_intents;
      "total_matches", `Int state_stats.total_matches_found;
      "total_settlements", `Int state_stats.total_settlements_completed;
    ];
    "matching", `Assoc [
      "rounds_completed", `Int matching_stats.rounds_completed;
      "matches_found", `Int matching_stats.total_matches_found;
      "matches_per_second", `Float matching_stats.matches_per_second;
    ];
    "network", `Assoc [
      "connected_peers", `Int peer_stats.connected_peers;
      "total_peers", `Int peer_stats.total_peers;
      "messages", `Int peer_stats.total_messages;
    ];
    "settlement", `Assoc [
      "executed", `Int settlement_stats.settlements_executed;
      "failed", `Int settlement_stats.settlements_failed;
      "success_rate", `Float settlement_stats.success_rate;
    ];
    "reputation", `Assoc [
      "average", `Float reputation_stats.average_reputation;
      "total_agents", `Int reputation_stats.total_agents;
    ];
  ]

(** {2 Node Lifecycle} *)

(** Start the node *)
let start node =
  if node.running then
    Error "Node already running"
  else begin
    node.running <- true;
    
    (* Connect to bootstrap nodes *)
    List.iter (fun bootstrap ->
      let endpoint = Transport.endpoint_from_uri bootstrap in
      let _ = Peer.Discovery.bootstrap_peers node.peer_manager [endpoint] in
      ()
    ) node.config.bootstrap_nodes;
    
    (* Start matching engine *)
    Engine.start node.matching_engine;
    
    (* Start gossip engine *)
    Gossip.start node.gossip_engine;
    
    (* Start peer connection maintenance *)
    let rec maintain_loop () =
      if node.running then begin
        Peer.maintain_connections node.peer_manager;
        Unix.sleep 30;
        maintain_loop ()
      end
    in
    let _ = Thread.create maintain_loop () in
    
    Ok ()
  end

(** Stop the node *)
let stop node =
  node.running <- false;
  Engine.stop node.matching_engine;
  Gossip.stop node.gossip_engine;
  Ok ()

(** {2 Advanced Operations} *)

module Advanced = struct
  (** Create a market maker bot *)
  let create_market_maker node resource_type spread_percentage =
    let rec market_make () =
      if node.running then begin
        (* Get current market state *)
        let intents = query_intents node ~resource_type:(Some resource_type) () in
        
        (* Calculate mid price *)
        let buy_prices = 
          intents
          |> List.filter (fun i -> i.Types.want_field.resource_type = resource_type)
          |> List.filter_map (fun i ->
                let ranges = Intent.get_price_ranges i in
                if List.length ranges > 0 then Some (List.nth ranges 0) else None)
          |> List.map fst
        in
        
        let sell_prices =
          intents
          |> List.filter (fun i -> i.Types.offer_field.resource_type = resource_type)
          |> List.filter_map (fun i ->
                let ranges = Intent.get_price_ranges i in
                if List.length ranges > 0 then Some (List.nth ranges 0) else None)
          |> List.map snd
        in
        
        match buy_prices, sell_prices with
        | [], [] -> ()  (* No market *)
        | _ ->
            let mid_price = 
              let avg lst = List.fold_left (+.) 0.0 lst /. float_of_int (List.length lst) in
              let buy_avg = if buy_prices = [] then 100.0 else avg buy_prices in
              let sell_avg = if sell_prices = [] then 100.0 else avg sell_prices in
              (buy_avg +. sell_avg) /. 2.0
            in
            
            (* Post buy and sell intents *)
            let buy_price = mid_price *. (1.0 -. spread_percentage /. 2.0) in
            let sell_price = mid_price *. (1.0 +. spread_percentage /. 2.0) in
            
            (* Create buy intent *)
            let _ = 
              Resource.create_field 
                ~resource_type:"currency:fiat:USD"
                ~min_quantity:buy_price
                ~max_quantity:buy_price
                ~quality:Types.Fungible
                ~metadata:[]
              |> Result.map (fun offers ->
                  Resource.create_field
                    ~resource_type:resource_type
                    ~min_quantity:1.0
                    ~max_quantity:1.0
                    ~quality:Types.Fungible
                    ~metadata:[]
                  |> Result.map (fun wants ->
                      post_intent node ~offers ~wants ()))
            in
            
            (* Create sell intent *)
            let _ = 
              Resource.create_field 
                ~resource_type:resource_type
                ~min_quantity:1.0
                ~max_quantity:1.0
                ~quality:Types.Fungible
                ~metadata:[]
              |> Result.map (fun offers ->
                  Resource.create_field
                    ~resource_type:"currency:fiat:USD"
                    ~min_quantity:sell_price
                    ~max_quantity:sell_price
                    ~quality:Types.Fungible
                    ~metadata:[]
                  |> Result.map (fun wants ->
                      post_intent node ~offers ~wants ()))
            in
            
            (* Sleep and repeat *)
            Unix.sleep 10;
            market_make ()
      end
    in
    Thread.create market_make ()
end