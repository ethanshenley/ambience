(** Peer Management Module
    
    This module manages peer connections, discovery, and reputation in the
    peer-to-peer network.
    
    Key concepts:
    - Peer discovery (bootstrap, mDNS, DHT)
    - Connection management
    - Peer reputation and scoring
    - Peer selection strategies
*)

open Ambience_core.Types
open Transport
open Protocol
module Intent = Ambience_core.Intent

(** Import capability type from trust module *)
type capability = Ambience_trust.Capability.capability

(** Peer information *)
type peer_info = {
  peer_id: public_key;
  endpoints: endpoint list;
  first_seen: timestamp;
  last_seen: timestamp;
  reputation_score: float;
  capabilities: capability list;
  metadata: (string * string) list;
  connection_state: peer_connection_state;
  statistics: peer_stats;
}

and peer_connection_state =
  | NotConnected
  | Connecting
  | Connected of connection
  | Disconnecting
  | Banned of string  (* Reason for ban *)

and peer_stats = {
  messages_sent: int;
  messages_received: int;
  bytes_sent: int;
  bytes_received: int;
  intents_shared: int;
  matches_found: int;
  failed_connections: int;
  average_latency: float;
}

(** Peer manager *)
type peer_manager = {
  my_id: public_key;
  peers: (public_key, peer_info) Hashtbl.t;
  connections: (public_key, connection) Hashtbl.t;
  connection_pool: Pool.t;
  router: router;
  
  (* Configuration *)
  max_peers: int;
  min_peers: int;
  peer_timeout: float;  (* Seconds before peer considered dead *)
  
  (* Callbacks *)
  mutable on_peer_connected: (peer_info -> unit) option;
  mutable on_peer_disconnected: (peer_info -> unit) option;
  mutable on_peer_discovered: (peer_info -> unit) option;
}

(** Create peer manager *)
let create_manager my_id ?(max_peers = 50) ?(min_peers = 5) () = {
  my_id = my_id;
  peers = Hashtbl.create max_peers;
  connections = Hashtbl.create max_peers;
  connection_pool = Pool.create ~max_connections:max_peers ();
  router = create_router ();
  max_peers = max_peers;
  min_peers = min_peers;
  peer_timeout = 300.0;  (* 5 minutes *)
  on_peer_connected = None;
  on_peer_disconnected = None;
  on_peer_discovered = None;
}

(** Create peer stats *)
let create_peer_stats () = {
  messages_sent = 0;
  messages_received = 0;
  bytes_sent = 0;
  bytes_received = 0;
  intents_shared = 0;
  matches_found = 0;
  failed_connections = 0;
  average_latency = 0.0;
}

(** {2 Peer Discovery} *)

module Discovery = struct
  (** Discovery method *)
  type method_ =
    | Bootstrap of endpoint list  (* Known bootstrap nodes *)
    | MDNS                       (* Multicast DNS for local network *)
    | DHT                        (* Distributed hash table *)
    | Exchange                   (* Peer exchange protocol *)
  
  (** Bootstrap from known peers *)
  let bootstrap_peers manager endpoints =
    List.iter (fun endpoint ->
      (* Create peer info *)
      let peer_id = "bootstrap_" ^ endpoint_to_string endpoint in
      let peer_info = {
        peer_id = peer_id;
        endpoints = [endpoint];
        first_seen = Ambience_core.Time_provider.now ();
        last_seen = Ambience_core.Time_provider.now ();
        reputation_score = 0.5;  (* Neutral *)
        capabilities = [];
        metadata = [];
        connection_state = NotConnected;
        statistics = create_peer_stats ();
      } in
      
      (* Add to manager *)
      Hashtbl.replace manager.peers peer_id peer_info;
      
      (* Trigger discovery callback *)
      (match manager.on_peer_discovered with
       | None -> ()
       | Some callback -> callback peer_info)
    ) endpoints
  
  (** Discover peers via mDNS *)
  let mdns_discovery _manager =
    (* TODO: Implement mDNS discovery *)
    (* Would use OCaml mDNS library to broadcast/listen *)
    ()
  
  (** Peer exchange - share peer lists *)
  let peer_exchange manager peer_id =
    (* Get our peer list *)
    let our_peers = 
      Hashtbl.fold (fun pid _info acc ->
        if pid <> peer_id then pid :: acc else acc
      ) manager.peers []
    in
    
    (* Send peer list to requesting peer *)
    (* TODO: Create and send peer exchange message *)
    ()
  
  (** Handle received peer list *)
  let handle_peer_list manager peer_list =
    List.iter (fun (peer_id, endpoints) ->
      if not (Hashtbl.mem manager.peers peer_id) then
        let peer_info = {
          peer_id = peer_id;
          endpoints = endpoints;
          first_seen = Ambience_core.Time_provider.now ();
          last_seen = Ambience_core.Time_provider.now ();
          reputation_score = 0.3;  (* Lower initial trust *)
          capabilities = [];
          metadata = [];
          connection_state = NotConnected;
          statistics = create_peer_stats ();
        } in
        Hashtbl.replace manager.peers peer_id peer_info
    ) peer_list
end

(** {2 Connection Management} *)

(** Connect to peer *)
let rec connect_to_peer manager peer_id =
  match Hashtbl.find_opt manager.peers peer_id with
  | None -> Error "Peer not found"
  | Some peer_info ->
      (* Check if already connected *)
      match peer_info.connection_state with
      | Connected _ -> Ok ()
      | Banned reason -> Error ("Peer banned: " ^ reason)
      | _ ->
          (* Try each endpoint *)
          let rec try_endpoints = function
            | [] -> Error "No endpoints available"
            | endpoint :: rest ->
                let conn = Transport.create endpoint in
                match Transport.connect conn with
                | Error _ -> try_endpoints rest
                | Ok () ->
                    (* Send hello message *)
                    let agent_info = {
                      agent_id = manager.my_id;
                      name = Some "ACP Node";
                      capabilities = ["binary"; "json"; "compression"];
                      reputation = 0.5;
                      metadata = [];
                    } in
                    let hello = create_hello manager.my_id agent_info in
                    let data = Serialization.to_binary hello in
                    
                    (match Transport.send conn data with
                     | Error e -> Error e
                     | Ok () ->
                         (* Update peer state *)
                         let updated_peer = {
                           peer_info with
                           connection_state = Connected conn;
                           last_seen = Ambience_core.Time_provider.now ();
                         } in
                         Hashtbl.replace manager.peers peer_id updated_peer;
                         Hashtbl.replace manager.connections peer_id conn;
                         
                         (* Set up message handler *)
                         Transport.on_message conn (fun data ->
                           handle_peer_message manager peer_id data
                         );
                         
                         (* Trigger callback *)
                         (match manager.on_peer_connected with
                          | None -> ()
                          | Some callback -> callback updated_peer);
                         
                         Ok ())
          in
          try_endpoints peer_info.endpoints

(** Handle message from peer *)
and handle_peer_message manager peer_id data =
  match Serialization.from_binary data with
  | None -> ()  (* Invalid message *)
  | Some msg ->
      (* Update peer stats *)
      (match Hashtbl.find_opt manager.peers peer_id with
       | None -> ()
       | Some peer ->
           let stats = peer.statistics in
           let updated_stats = {
             stats with
             messages_received = stats.messages_received + 1;
             bytes_received = stats.bytes_received + Bytes.length data;
           } in
           let updated_peer = {
             peer with
             statistics = updated_stats;
             last_seen = Ambience_core.Time_provider.now ();
           } in
           Hashtbl.replace manager.peers peer_id updated_peer);
      
      (* Route message *)
      route_message manager.router msg

(** Disconnect from peer *)
let disconnect_from_peer manager peer_id =
  match Hashtbl.find_opt manager.peers peer_id with
  | None -> ()
  | Some peer_info ->
      (match peer_info.connection_state with
       | Connected conn ->
           (* Send goodbye *)
           let goodbye = Protocol.create_goodbye manager.my_id "Disconnecting" in
           let data = Serialization.to_binary goodbye in
           let _ = Transport.send conn data in
           
           (* Close connection *)
           Transport.close conn;
           
           (* Update state *)
           let updated_peer = {
             peer_info with
             connection_state = NotConnected;
           } in
           Hashtbl.replace manager.peers peer_id updated_peer;
           Hashtbl.remove manager.connections peer_id;
           
           (* Trigger callback *)
           (match manager.on_peer_disconnected with
            | None -> ()
            | Some callback -> callback updated_peer)
       | _ -> ())

(** {2 Peer Selection} *)

(** Peer selection strategy *)
type selection_strategy =
  | Random              (* Random selection *)
  | HighestReputation   (* Prefer high reputation peers *)
  | LowestLatency      (* Prefer low latency peers *)
  | MostActive         (* Prefer active peers *)
  | Balanced           (* Balance multiple factors *)

(** Select peers for broadcast *)
let select_peers manager strategy count =
  let connected_peers = 
    Hashtbl.fold (fun peer_id peer_info acc ->
      match peer_info.connection_state with
      | Connected _ -> (peer_id, peer_info) :: acc
      | _ -> acc
    ) manager.peers []
  in
  
  let sorted = 
    match strategy with
    | Random ->
        (* Shuffle *)
        List.sort (fun _ _ -> Random.int 3 - 1) connected_peers
    
    | HighestReputation ->
        List.sort (fun (_, p1) (_, p2) ->
          Float.compare p2.reputation_score p1.reputation_score
        ) connected_peers
    
    | LowestLatency ->
        List.sort (fun (_, p1) (_, p2) ->
          Float.compare p1.statistics.average_latency p2.statistics.average_latency
        ) connected_peers
    
    | MostActive ->
        List.sort (fun (_, p1) (_, p2) ->
          let activity1 = p1.statistics.messages_received + p1.statistics.messages_sent in
          let activity2 = p2.statistics.messages_received + p2.statistics.messages_sent in
          Int.compare activity2 activity1
        ) connected_peers
    
    | Balanced ->
        (* Score based on multiple factors *)
        List.sort (fun (_, p1) (_, p2) ->
          let score p =
            p.reputation_score *. 0.4 +.
            (1.0 /. (1.0 +. p.statistics.average_latency)) *. 0.3 +.
            (float_of_int (p.statistics.messages_received) /. 1000.0) *. 0.3
          in
          Float.compare (score p2) (score p1)
        ) connected_peers
  in
  
  (* Take requested count *)
  List.filteri (fun i _ -> i < count) sorted |> List.map fst

(** {2 Reputation Management} *)

module Reputation = struct
  (** Reputation event *)
  type event =
    | SuccessfulConnection
    | FailedConnection
    | ValidMessageReceived
    | InvalidMessageReceived
    | IntentShared
    | MatchFound
    | SettlementCompleted
    | MaliciousBehavior of string
  
  (** Update peer reputation *)
  let update_reputation manager peer_id event =
    match Hashtbl.find_opt manager.peers peer_id with
    | None -> ()
    | Some peer ->
        let delta = 
          match event with
          | SuccessfulConnection -> 0.01
          | FailedConnection -> -0.02
          | ValidMessageReceived -> 0.001
          | InvalidMessageReceived -> -0.005
          | IntentShared -> 0.002
          | MatchFound -> 0.01
          | SettlementCompleted -> 0.02
          | MaliciousBehavior _ -> -0.5
        in
        
        let new_score = max 0.0 (min 1.0 (peer.reputation_score +. delta)) in
        
        (* Ban if reputation too low *)
        let new_state = 
          if new_score < 0.1 then
            Banned "Low reputation"
          else
            peer.connection_state
        in
        
        let updated_peer = {
          peer with
          reputation_score = new_score;
          connection_state = new_state;
        } in
        
        Hashtbl.replace manager.peers peer_id updated_peer;
        
        (* Disconnect if banned *)
        if new_state = Banned "Low reputation" then
          disconnect_from_peer manager peer_id
  
  (** Get reputation score *)
  let get_reputation manager peer_id =
    match Hashtbl.find_opt manager.peers peer_id with
    | None -> 0.0
    | Some peer -> peer.reputation_score
end

(** {2 Maintenance} *)

(** Maintain peer connections *)
let maintain_connections manager =
  let current_time = Ambience_core.Time_provider.now () in
  
  (* Check for dead peers *)
  Hashtbl.iter (fun peer_id peer ->
    let age = current_time -. peer.last_seen in
    if age > manager.peer_timeout then
      (* Peer is dead, disconnect *)
      disconnect_from_peer manager peer_id
  ) manager.peers;
  
  (* Ensure minimum connections *)
  let connected_count = 
    Hashtbl.fold (fun _ peer acc ->
      match peer.connection_state with
      | Connected _ -> acc + 1
      | _ -> acc
    ) manager.peers 0
  in
  
  if connected_count < manager.min_peers then
    (* Connect to more peers *)
    let unconnected = 
      Hashtbl.fold (fun peer_id peer acc ->
        match peer.connection_state with
        | NotConnected -> peer_id :: acc
        | _ -> acc
      ) manager.peers []
    in
    
    List.iter (fun peer_id ->
      let _ = connect_to_peer manager peer_id in
      ()
    ) (List.filteri (fun i _ -> 
        i < (manager.min_peers - connected_count)
      ) unconnected)

(** Send data to a specific peer *)
let send_to_peer manager peer_id data =
  match Hashtbl.find_opt manager.connections peer_id with
  | None -> Error "Peer not connected"
  | Some conn -> Transport.send conn data

(** Get manager statistics *)
type manager_stats = {
  total_peers: int;
  connected_peers: int;
  banned_peers: int;
  average_reputation: float;
  total_messages: int;
  total_bytes: int;
}

let get_manager_stats manager =
  let stats = 
    Hashtbl.fold (fun _ peer acc ->
      let connected = 
        match peer.connection_state with 
        | Connected _ -> 1 
        | _ -> 0
      in
      let banned = 
        match peer.connection_state with 
        | Banned _ -> 1 
        | _ -> 0
      in
      {
        total_peers = acc.total_peers + 1;
        connected_peers = acc.connected_peers + connected;
        banned_peers = acc.banned_peers + banned;
        average_reputation = acc.average_reputation +. peer.reputation_score;
        total_messages = acc.total_messages + peer.statistics.messages_received + 
                        peer.statistics.messages_sent;
        total_bytes = acc.total_bytes + peer.statistics.bytes_received + 
                     peer.statistics.bytes_sent;
      }
    ) manager.peers {
      total_peers = 0;
      connected_peers = 0;
      banned_peers = 0;
      average_reputation = 0.0;
      total_messages = 0;
      total_bytes = 0;
    }
  in
  
  { stats with
    average_reputation = 
      if stats.total_peers > 0 then
        stats.average_reputation /. float_of_int stats.total_peers
      else 0.0
  }