(** Transport Layer Module
    
    This module provides an abstraction over different network transports
    (TCP, UDP, WebSocket, etc.) allowing the protocol to work over any
    communication medium.
    
    Key principles:
    - Transport agnostic protocol
    - Pluggable transport implementations
    - Connection pooling and management
    - Automatic reconnection and failover
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent

(** Transport types *)
type transport_type = 
  | TCP
  | UDP  
  | WebSocket
  | HTTP
  | QUIC
  | Memory  (* For testing *)

(** Connection state *)
type connection_state =
  | Disconnected
  | Connecting
  | Connected
  | Disconnecting
  | Failed of string

(** Network endpoint *)
type endpoint = {
  transport: transport_type;
  host: string;
  port: int;
  path: string option;  (* For HTTP/WebSocket *)
}

(** Connection statistics *)
type connection_stats = {
  bytes_sent: int;
  bytes_received: int;
  messages_sent: int;
  messages_received: int;
  connected_since: timestamp option;
  latency_ms: float option;
  packet_loss: float;
}

(** Abstract connection type *)
type connection = {
  id: uuid;
  endpoint: endpoint;
  mutable state: connection_state;
  mutable stats: connection_stats;
  (* Callbacks *)
  mutable on_message: (bytes -> unit) option;
  mutable on_error: (string -> unit) option;
  mutable on_close: (unit -> unit) option;
  (* Implementation specific data *)
  impl: connection_impl;
}

and connection_impl =
  | TCPImpl of tcp_connection
  | UDPImpl of udp_connection
  | WebSocketImpl of ws_connection
  | MemoryImpl of memory_connection

and tcp_connection = {
  mutable tcp_socket: Unix.file_descr option;  (* Renamed to avoid conflict *)
  mutable reader_thread: Thread.t option;
  read_buffer: Buffer.t;
  write_buffer: Buffer.t;
  mutable keep_alive: bool;
}

and udp_connection = {
  mutable udp_socket: Unix.file_descr option;  (* Renamed to avoid conflict *)
  sequence_number: int ref;
  ack_map: (int, timestamp) Hashtbl.t;  (* For reliability *)
}

and ws_connection = {
  mutable ws: unit;  (* Websocket library specific *)
}

and memory_connection = {
  inbox: (bytes Queue.t) ref;
  outbox: (bytes Queue.t) ref;
  peer_id: string option ref;
}

(** Create endpoint from URI string *)
let endpoint_from_uri uri_str =
  (* Parse URI like "tcp://localhost:8080" or "ws://example.com:9090/path" *)
  let uri = Uri.of_string uri_str in
  let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri |> Option.value ~default:8080 in
  let path = Uri.path uri in
  
  let transport = 
    match scheme with
    | "tcp" -> TCP
    | "udp" -> UDP
    | "ws" | "wss" -> WebSocket
    | "http" | "https" -> HTTP
    | "quic" -> QUIC
    | "memory" -> Memory
    | _ -> TCP  (* Default *)
  in
  
  {
    transport = transport;
    host = host;
    port = port;
    path = if path = "" then None else Some path;
  }

(** Endpoint to string *)
let endpoint_to_string endpoint =
  let scheme = 
    match endpoint.transport with
    | TCP -> "tcp"
    | UDP -> "udp"
    | WebSocket -> "ws"
    | HTTP -> "http"
    | QUIC -> "quic"
    | Memory -> "memory"
  in
  
  let path = Option.value ~default:"" endpoint.path in
  Printf.sprintf "%s://%s:%d%s" scheme endpoint.host endpoint.port path

(** {2 Connection Management} *)

(** Create connection stats *)
let create_stats () = {
  bytes_sent = 0;
  bytes_received = 0;
  messages_sent = 0;
  messages_received = 0;
  connected_since = None;
  latency_ms = None;
  packet_loss = 0.0;
}

(** {2 TCP Implementation} *)

module TCP = struct
  (** Create TCP socket *)
  let create_socket endpoint =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    Unix.setsockopt socket Unix.SO_KEEPALIVE true;
    
    (* Set non-blocking mode *)
    Unix.set_nonblock socket;
    
    socket
  
  (** Connect to TCP endpoint *)
  let connect endpoint socket =
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string endpoint.host, endpoint.port) in
    try
      Unix.connect socket addr;
      Ok ()
    with
    | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
        (* Connection in progress (non-blocking) *)
        Ok ()
    | Unix.Unix_error (err, _, _) ->
        Error (Unix.error_message err)
  
  (** Reader thread *)
  let reader_loop connection socket =
    let buffer = Bytes.create 4096 in
    let tcp_impl = 
      match connection.impl with
      | TCPImpl impl -> impl
      | _ -> failwith "Invalid implementation"
    in
    
    while connection.state = Connected do
      try
        let n = Unix.read socket buffer 0 4096 in
        if n = 0 then
          (* Connection closed *)
          connection.state <- Disconnected
        else begin
          (* Add to read buffer *)
          Buffer.add_bytes tcp_impl.read_buffer (Bytes.sub buffer 0 n);
          connection.stats <- {
            connection.stats with
            bytes_received = connection.stats.bytes_received + n;
            messages_received = connection.stats.messages_received + 1;
          };
          
          (* Trigger callback *)
          match connection.on_message with
          | None -> ()
          | Some callback ->
              let data = Buffer.to_bytes tcp_impl.read_buffer in
              Buffer.clear tcp_impl.read_buffer;
              callback data
        end
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (* No data available, retry *)
          Unix.sleepf 0.001
      | Unix.Unix_error (err, _, _) ->
          connection.state <- Failed (Unix.error_message err);
          (match connection.on_error with
           | None -> ()
           | Some callback -> callback (Unix.error_message err))
    done
  
  (** Send data over TCP *)
  let send connection data =
    match connection.impl with
    | TCPImpl impl ->
        (match impl.tcp_socket with
         | None -> Error "Not connected"
         | Some socket ->
             try
               let len = Bytes.length data in
               let sent = Unix.write socket data 0 len in
               connection.stats <- {
                 connection.stats with
                 bytes_sent = connection.stats.bytes_sent + sent;
                 messages_sent = connection.stats.messages_sent + 1;
               };
               Ok ()
             with Unix.Unix_error (err, _, _) ->
               Error (Unix.error_message err))
    | _ -> Error "Invalid implementation"
end

(** {2 UDP Implementation} *)

module UDP = struct
  (** Create UDP socket *)
  let create_socket endpoint =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    
    (* Bind to local address *)
    let bind_addr = Unix.ADDR_INET (Unix.inet_addr_any, 0) in
    Unix.bind socket bind_addr;
    
    socket
  
  (** Send datagram *)
  let send connection data =
    match connection.impl with
    | UDPImpl impl ->
        (match impl.udp_socket with
         | None -> Error "Not connected"
         | Some socket ->
             try
               let addr = Unix.ADDR_INET (
                 Unix.inet_addr_of_string connection.endpoint.host,
                 connection.endpoint.port
               ) in
               
               (* Add sequence number for reliability *)
               let seq = !(impl.sequence_number) in
               impl.sequence_number := seq + 1;
               
               (* Prepend sequence number to data *)
               let packet = Bytes.create (4 + Bytes.length data) in
               Bytes.set_int32_be packet 0 (Int32.of_int seq);
               Bytes.blit data 0 packet 4 (Bytes.length data);
               
               let sent = Unix.sendto socket packet 0 
                           (Bytes.length packet) [] addr in
               
               connection.stats <- {
                 connection.stats with
                 bytes_sent = connection.stats.bytes_sent + sent;
                 messages_sent = connection.stats.messages_sent + 1;
               };
               
               (* Track for ACK *)
               Hashtbl.add impl.ack_map seq (Unix.time ());
               
               Ok ()
             with Unix.Unix_error (err, _, _) ->
               Error (Unix.error_message err))
    | _ -> Error "Invalid implementation"
end

(** {2 Memory Implementation (for testing)} *)

module Memory = struct
  (* Global message queue for in-memory transport *)
  let global_queues : (string, bytes Queue.t) Hashtbl.t = Hashtbl.create 10
  
  (** Get or create queue for peer *)
  let get_queue peer_id =
    try Hashtbl.find global_queues peer_id
    with Not_found ->
      let queue = Queue.create () in
      Hashtbl.add global_queues peer_id queue;
      queue
  
  (** Send to memory queue *)
  let send connection data =
    match connection.impl with
    | MemoryImpl impl ->
        (match !(impl.peer_id) with
         | None -> Error "No peer connected"
         | Some peer ->
             let queue = get_queue peer in
             Queue.push data queue;
             
             connection.stats <- {
               connection.stats with
               bytes_sent = connection.stats.bytes_sent + Bytes.length data;
               messages_sent = connection.stats.messages_sent + 1;
             };
             
             Ok ())
    | _ -> Error "Invalid implementation"
  
  (** Receive from memory queue *)
  let receive connection =
    match connection.impl with
    | MemoryImpl impl ->
        if Queue.is_empty !(impl.inbox) then
          None
        else
          let data = Queue.pop !(impl.inbox) in
          connection.stats <- {
            connection.stats with
            bytes_received = connection.stats.bytes_received + Bytes.length data;
            messages_received = connection.stats.messages_received + 1;
          };
          Some data
    | _ -> None
end

(** {2 Connection Pool} *)

module Pool = struct
  type t = {
    connections: (string, connection) Hashtbl.t;  (* endpoint -> connection *)
    max_connections: int;
    mutable active_count: int;
  }
  
  (** Create connection pool *)
  let create ?(max_connections = 100) () = {
    connections = Hashtbl.create max_connections;
    max_connections = max_connections;
    active_count = 0;
  }
  
  (** Get or create connection *)
  let get_connection pool endpoint =
    let key = endpoint_to_string endpoint in
    try
      let conn = Hashtbl.find pool.connections key in
      if conn.state = Connected then
        Ok conn
      else
        Error "Connection not active"
    with Not_found ->
      if pool.active_count >= pool.max_connections then
        Error "Connection pool full"
      else
        Error "Connection not found"
  
  (** Add connection to pool *)
  let add_connection pool connection =
    let key = endpoint_to_string connection.endpoint in
    if pool.active_count < pool.max_connections then begin
      Hashtbl.replace pool.connections key connection;
      pool.active_count <- pool.active_count + 1;
      Ok ()
    end else
      Error "Connection pool full"
  
  (** Remove connection from pool *)
  let remove_connection pool endpoint =
    let key = endpoint_to_string endpoint in
    if Hashtbl.mem pool.connections key then begin
      Hashtbl.remove pool.connections key;
      pool.active_count <- pool.active_count - 1
    end

  (** Pool statistics type *)
  type pool_stats = {
    active_connections: int;
    total_connections: int;
    bytes_sent: int;
    bytes_received: int;
  }

  (** Get pool statistics *)
  let get_stats pool =
    let total_bytes_sent = ref 0 in
    let total_bytes_received = ref 0 in

    Hashtbl.iter (fun _key conn ->
      total_bytes_sent := !total_bytes_sent + conn.stats.bytes_sent;
      total_bytes_received := !total_bytes_received + conn.stats.bytes_received
    ) pool.connections;

    {
      active_connections = pool.active_count;
      total_connections = pool.max_connections;
      bytes_sent = !total_bytes_sent;
      bytes_received = !total_bytes_received;
    }
end

(** {2 Main Connection Interface} *)

(** Create a new connection *)
let create endpoint =
  let impl = 
    match endpoint.transport with
    | TCP ->
        TCPImpl {
          tcp_socket = None;
          reader_thread = None;
          read_buffer = Buffer.create 4096;
          write_buffer = Buffer.create 4096;
          keep_alive = true;
        }
    | UDP ->
        UDPImpl {
          udp_socket = None;
          sequence_number = ref 0;
          ack_map = Hashtbl.create 100;
        }
    | WebSocket ->
        WebSocketImpl { ws = () }
    | Memory ->
        MemoryImpl {
          inbox = ref (Queue.create ());
          outbox = ref (Queue.create ());
          peer_id = ref None;
        }
    | _ ->
        failwith "Transport not implemented"
  in
  
  {
    id = Intent.generate_uuid ();
    endpoint = endpoint;
    state = Disconnected;
    stats = create_stats ();
    on_message = None;
    on_error = None;
    on_close = None;
    impl = impl;
  }

(** Connect to endpoint *)
let connect connection =
  connection.state <- Connecting;
  
  match connection.impl with
  | TCPImpl impl ->
      let socket = TCP.create_socket connection.endpoint in
      impl.tcp_socket <- Some socket;
      
      (match TCP.connect connection.endpoint socket with
       | Ok () ->
           connection.state <- Connected;
           connection.stats <- 
             { connection.stats with connected_since = Some (Unix.time ()) };
           
           (* Start reader thread *)
           let thread = Thread.create (TCP.reader_loop connection) socket in
           impl.reader_thread <- Some thread;
           
           Ok ()
       | Error e ->
           connection.state <- Failed e;
           Error e)
  
  | UDPImpl impl ->
      let socket = UDP.create_socket connection.endpoint in
      impl.udp_socket <- Some socket;
      connection.state <- Connected;
      connection.stats <- 
        { connection.stats with connected_since = Some (Unix.time ()) };
      Ok ()
  
  | MemoryImpl _impl ->
      connection.state <- Connected;
      connection.stats <- 
        { connection.stats with connected_since = Some (Unix.time ()) };
      Ok ()
  
  | _ ->
      Error "Transport not implemented"

(** Send data over connection *)
let send connection data =
  if connection.state <> Connected then
    Error "Not connected"
  else
    match connection.impl with
    | TCPImpl _ -> TCP.send connection data
    | UDPImpl _ -> UDP.send connection data
    | MemoryImpl _ -> Memory.send connection data
    | _ -> Error "Transport not implemented"

(** Receive data (for polling transports) *)
let receive connection =
  match connection.impl with
  | MemoryImpl _ -> Memory.receive connection
  | _ -> None  (* Other transports use callbacks *)

(** Close connection *)
let close connection =
  connection.state <- Disconnecting;
  
  (* Call close callback *)
  (match connection.on_close with
   | None -> ()
   | Some callback -> callback ());
  
  (* Close implementation *)
  (match connection.impl with
   | TCPImpl impl ->
       (match impl.tcp_socket with
        | None -> ()
        | Some socket -> Unix.close socket);
       impl.tcp_socket <- None
   | UDPImpl impl ->
       (match impl.udp_socket with
        | None -> ()
        | Some socket -> Unix.close socket);
       impl.udp_socket <- None
   | _ -> ());
  
  connection.state <- Disconnected

(** Get connection statistics *)
let get_stats connection = connection.stats

(** Set message callback *)
let on_message connection callback =
  connection.on_message <- Some callback

(** Set error callback *)
let on_error connection callback =
  connection.on_error <- Some callback

(** Set close callback *)
let on_close connection callback =
  connection.on_close <- Some callback