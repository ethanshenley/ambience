(** Gossip Protocol Module
    
    This module implements epidemic-style gossip propagation for distributing
    intents and matches throughout the network.
    
    Key concepts:
    - Epidemic propagation with controlled infection
    - Message deduplication
    - TTL and hop counting
    - Probabilistic forwarding
    - Pull and push gossip
*)

open Ambience_core.Types
open Protocol
open Peer

(** Gossip message *)
type gossip_message = {
  content: protocol_message;
  message_id: uuid;
  origin: public_key;
  hop_count: int;
  max_hops: int;
  timestamp: timestamp;
  seen_by: public_key list;
}

(** Gossip configuration *)
type config = {
  fanout: int;              (* Number of peers to forward to *)
  max_hops: int;            (* Maximum hops before dropping *)
  cache_size: int;          (* Size of seen message cache *)
  cache_ttl: float;         (* How long to remember messages *)
  forward_probability: float; (* Probability of forwarding *)
  pull_interval: float;     (* Interval for pull gossip *)
}

(** Default configuration *)
let default_config = {
  fanout = 3;
  max_hops = 5;
  cache_size = 10000;
  cache_ttl = 600.0;  (* 10 minutes *)
  forward_probability = 0.8;
  pull_interval = 30.0;  (* 30 seconds *)
}

(** Gossip engine *)
type gossip_engine = {
  config: config;
  peer_manager: peer_manager;
  seen_messages: (uuid, timestamp) Hashtbl.t;
  message_cache: (uuid, gossip_message) Hashtbl.t;
  mutable running: bool;
  mutable messages_sent: int;
  mutable messages_received: int;
  mutable messages_dropped: int;
}

(** Create gossip engine *)
let create_engine peer_manager ?(config = default_config) () = {
  config = config;
  peer_manager = peer_manager;
  seen_messages = Hashtbl.create config.cache_size;
  message_cache = Hashtbl.create config.cache_size;
  running = false;
  messages_sent = 0;
  messages_received = 0;
  messages_dropped = 0;
}

(** {2 Message Management} *)

(** Check if message was already seen *)
let is_duplicate engine msg_id =
  Hashtbl.mem engine.seen_messages msg_id

(** Mark message as seen *)
let mark_seen engine msg_id =
  Hashtbl.replace engine.seen_messages msg_id (Unix.time ())

(** Clean old messages from cache *)
let clean_cache engine =
  let current_time = Unix.time () in
  let old_threshold = current_time -. engine.config.cache_ttl in
  
  (* Remove old seen messages *)
  let to_remove = ref [] in
  Hashtbl.iter (fun msg_id timestamp ->
    if timestamp < old_threshold then
      to_remove := msg_id :: !to_remove
  ) engine.seen_messages;
  
  List.iter (Hashtbl.remove engine.seen_messages) !to_remove;
  
  (* Remove old cached messages *)
  let to_remove = ref [] in
  Hashtbl.iter (fun msg_id msg ->
    if msg.timestamp < old_threshold then
      to_remove := msg_id :: !to_remove
  ) engine.message_cache;
  
  List.iter (Hashtbl.remove engine.message_cache) !to_remove

(** {2 Push Gossip} *)

(** Propagate message via push gossip *)
let push_gossip engine gossip_msg =
  (* Check if we should forward *)
  if gossip_msg.hop_count >= gossip_msg.max_hops then begin
    engine.messages_dropped <- engine.messages_dropped + 1;
    ()
  end else if Random.float 1.0 > engine.config.forward_probability then begin
    (* Probabilistic drop *)
    engine.messages_dropped <- engine.messages_dropped + 1;
    ()
  end else begin
    (* Select peers to forward to *)
    let peers = select_peers engine.peer_manager Balanced engine.config.fanout in
    
    (* Update message for forwarding *)
    let forwarded_msg = {
      gossip_msg with
      hop_count = gossip_msg.hop_count + 1;
      seen_by = engine.peer_manager.my_id :: gossip_msg.seen_by;
    } in
    
    (* Send to selected peers *)
    List.iter (fun peer_id ->
      if not (List.mem peer_id forwarded_msg.seen_by) then
        match Hashtbl.find_opt engine.peer_manager.connections peer_id with
        | None -> ()
        | Some conn ->
            let data = Serialization.to_binary forwarded_msg.content in
            (match Transport.send conn data with
             | Ok () -> 
                 engine.messages_sent <- engine.messages_sent + 1
             | Error _ -> ())
    ) peers
  end

(** Broadcast new message *)
let broadcast engine content =
  let gossip_msg = {
    content = content;
    message_id = content.msg_id;
    origin = engine.peer_manager.my_id;
    hop_count = 0;
    max_hops = engine.config.max_hops;
    timestamp = Unix.time ();
    seen_by = [engine.peer_manager.my_id];
  } in
  
  (* Mark as seen *)
  mark_seen engine gossip_msg.message_id;
  
  (* Cache message *)
  Hashtbl.replace engine.message_cache gossip_msg.message_id gossip_msg;
  
  (* Start propagation *)
  push_gossip engine gossip_msg

(** {2 Pull Gossip} *)

(** Request missing messages from peers *)
let pull_gossip engine =
  (* Get list of known message IDs *)
  let known_ids = 
    Hashtbl.fold (fun msg_id _ acc -> msg_id :: acc) 
      engine.seen_messages []
  in
  
  (* Select random peer *)
  let peers = select_peers engine.peer_manager Random 1 in
  
  match peers with
  | [] -> ()
  | peer_id :: _ ->
      (* Create pull request *)
      let query = {
        msg_id = Intent.generate_uuid ();
        msg_type = QueryMsg;
        sender = engine.peer_manager.my_id;
        timestamp = Unix.time ();
        payload = Query {
          query_id = Intent.generate_uuid ();
          query_type = QueryIntents;  (* Simplified *)
          filters = [];
          limit = Some 100;
        };
        signature = None;
      } in
      
      (* Send to peer *)
      (match Hashtbl.find_opt engine.peer_manager.connections peer_id with
       | None -> ()
       | Some conn ->
           let data = Serialization.to_binary query in
           let _ = Transport.send conn data in
           ())

(** Handle pull request *)
let handle_pull_request engine peer_id request_msg =
  (* Get messages we have that peer might not *)
  let our_messages = 
    Hashtbl.fold (fun _ msg acc ->
      if not (List.mem peer_id msg.seen_by) then
        msg :: acc
      else
        acc
    ) engine.message_cache []
  in
  
  (* Send messages to peer *)
  List.iter (fun msg ->
    match Hashtbl.find_opt engine.peer_manager.connections peer_id with
    | None -> ()
    | Some conn ->
        let data = Serialization.to_binary msg.content in
        let _ = Transport.send conn data in
        engine.messages_sent <- engine.messages_sent + 1
  ) (List.filteri (fun i _ -> i < 10) our_messages)  (* Limit to 10 *)

(** {2 Message Reception} *)

(** Handle received gossip message *)
let handle_received engine gossip_msg =
  engine.messages_received <- engine.messages_received + 1;
  
  (* Check if duplicate *)
  if is_duplicate engine gossip_msg.message_id then
    ()  (* Already seen, ignore *)
  else begin
    (* Mark as seen *)
    mark_seen engine gossip_msg.message_id;
    
    (* Cache message *)
    Hashtbl.replace engine.message_cache gossip_msg.message_id gossip_msg;
    
    (* Process the content *)
    route_message engine.peer_manager.router gossip_msg.content;
    
    (* Forward via push gossip *)
    push_gossip engine gossip_msg
  end

(** {2 Gossip Patterns} *)

(** Gossip strategy *)
type strategy =
  | Flood           (* Send to all peers *)
  | Random          (* Random subset *)
  | Proximity       (* Prefer nearby peers *)
  | Gradient        (* Follow gradient/direction *)

(** Apply gossip strategy *)
let apply_strategy engine strategy message =
  let peers = 
    match strategy with
    | Flood ->
        (* Get all connected peers *)
        select_peers engine.peer_manager Random 1000
    
    | Random ->
        (* Random subset *)
        select_peers engine.peer_manager Random engine.config.fanout
    
    | Proximity ->
        (* Prefer low latency peers *)
        select_peers engine.peer_manager LowestLatency engine.config.fanout
    
    | Gradient ->
        (* For directed gossip - e.g., towards peers interested in resource *)
        select_peers engine.peer_manager MostActive engine.config.fanout
  in
  
  (* Send to selected peers *)
  List.iter (fun peer_id ->
    broadcast engine message
  ) peers

(** {2 Engine Control} *)

(** Start gossip engine *)
let start engine =
  engine.running <- true;
  
  (* Start pull gossip timer *)
  let rec pull_loop () =
    if engine.running then begin
      Unix.sleepf engine.config.pull_interval;
      pull_gossip engine;
      pull_loop ()
    end
  in
  
  (* Start cache cleaner *)
  let rec clean_loop () =
    if engine.running then begin
      Unix.sleepf 60.0;  (* Clean every minute *)
      clean_cache engine;
      clean_loop ()
    end
  in
  
  (* Run in separate threads in production *)
  let _ = Thread.create pull_loop () in
  let _ = Thread.create clean_loop () in
  ()

(** Stop gossip engine *)
let stop engine =
  engine.running <- false

(** Get gossip statistics *)
type stats = {
  messages_sent: int;
  messages_received: int;
  messages_dropped: int;
  cache_size: int;
  seen_messages: int;
  cache_hit_rate: float;
}

let get_stats engine =
  let cache_size = Hashtbl.length engine.message_cache in
  let seen_size = Hashtbl.length engine.seen_messages in
  
  let hit_rate = 
    if engine.messages_received > 0 then
      float_of_int engine.messages_dropped /. 
      float_of_int engine.messages_received
    else
      0.0
  in
  
  {
    messages_sent = engine.messages_sent;
    messages_received = engine.messages_received;
    messages_dropped = engine.messages_dropped;
    cache_size = cache_size;
    seen_messages = seen_size;
    cache_hit_rate = hit_rate;
  }

(** {2 Advanced Gossip Algorithms} *)

module Advanced = struct
  (** Plumtree - optimized gossip tree *)
  type plumtree = {
    eager_peers: (public_key, bool) Hashtbl.t;  (* Push immediately *)
    lazy_peers: (public_key, bool) Hashtbl.t;   (* Send only ID *)
  }
  
  let create_plumtree () = {
    eager_peers = Hashtbl.create 10;
    lazy_peers = Hashtbl.create 50;
  }
  
  (** HyParView - hybrid partial view *)
  type hyparview = {
    active_view: public_key list;   (* Small, symmetric *)
    passive_view: public_key list;  (* Large, asymmetric *)
  }
  
  (** T-Man - topology management *)
  type tman = {
    ranking_function: peer_info -> peer_info -> int;
    view_size: int;
  }
end