(** Network Protocol Module
    
    This module implements the Ambient Commerce Protocol wire format and
    message handling. It handles serialization, deserialization, and
    protocol-level concerns like versioning and message routing.
    
    Key concepts:
    - Binary and JSON message formats
    - Protocol versioning and negotiation
    - Message compression and encryption
    - Rate limiting and flow control
*)

open Ambience_core
open Types

(** Protocol version *)
let protocol_version = "ACP/1.0"

(** Message types *)
type msg_type =
  | Hello           (* Initial handshake *)
  | HelloAck        (* Handshake acknowledgment *)
  | IntentMsg       (* Intent broadcast *)
  | MatchMsg        (* Match discovery notification *)
  | NegotiateMsg    (* Negotiation message *)
  | SettleMsg       (* Settlement notification *)
  | QueryMsg        (* Query for intents/matches *)
  | ResponseMsg     (* Response to query *)
  | HeartbeatMsg    (* Keep-alive *)
  | GoodbyeMsg      (* Graceful disconnect *)

(** Protocol message *)
type protocol_message = {
  msg_id: uuid;
  msg_type: msg_type;
  sender: public_key;
  timestamp: timestamp;
  payload: payload;
  signature: bytes option;  (* Optional signature *)
}

and payload =
  | Hello of hello_payload
  | HelloAck of hello_ack_payload
  | Intent of intent
  | Match of match_t
  | Negotiate of negotiation_payload
  | Settle of settlement
  | Query of query_payload
  | Response of response_payload
  | Heartbeat of heartbeat_payload
  | Goodbye of string  (* Reason *)

and hello_payload = {
  version: string;
  capabilities: string list;
  agent_info: agent_info;
}

and hello_ack_payload = {
  version: string;
  accepted: bool;
  reason: string option;
}

and negotiation_payload = {
  match_id: uuid;
  round: int;
  proposal: settlement_point option;
  accept: bool;
}

and query_payload = {
  query_id: uuid;
  query_type: query_type;
  filters: (string * string) list;
  limit: int option;
}

and query_type =
  | QueryIntents
  | QueryMatches
  | QuerySettlements
  | QueryAgents

and response_payload = {
  query_id: uuid;
  results: query_results;
  has_more: bool;
}

and query_results =
  | IntentResults of intent list
  | MatchResults of match_t list
  | SettlementResults of settlement list
  | AgentResults of agent_info list

and heartbeat_payload = {
  sequence: int;
  load: float;  (* Current load 0.0-1.0 *)
  active_intents: int;
  active_matches: int;
}

and agent_info = {
  agent_id: public_key;
  name: string option;
  capabilities: string list;
  reputation: float;
  metadata: (string * string) list;
}

(** {2 Serialization} *)

module Serialization = struct
  (** Message type to byte *)
  let msg_type_to_byte (t : msg_type) = match t with
    | Hello -> '\x01'
    | HelloAck -> '\x02'
    | IntentMsg -> '\x03'
    | MatchMsg -> '\x04'
    | NegotiateMsg -> '\x05'
    | SettleMsg -> '\x06'
    | QueryMsg -> '\x07'
    | ResponseMsg -> '\x08'
    | HeartbeatMsg -> '\x09'
    | GoodbyeMsg -> '\x0A'
  
  (** Byte to message type *)
  let byte_to_msg_type = function
    | '\x01' -> Some (Hello : msg_type)
    | '\x02' -> Some (HelloAck : msg_type)
    | '\x03' -> Some IntentMsg
    | '\x04' -> Some MatchMsg
    | '\x05' -> Some NegotiateMsg
    | '\x06' -> Some SettleMsg
    | '\x07' -> Some QueryMsg
    | '\x08' -> Some ResponseMsg
    | '\x09' -> Some HeartbeatMsg
    | '\x0A' -> Some GoodbyeMsg
    | _ -> None
  
  (** Serialize to JSON *)
  let to_json msg =
    `Assoc [
      "msg_id", `String msg.msg_id;
      "msg_type", `String (
        match (msg.msg_type : msg_type) with
        | Hello -> "hello"
        | HelloAck -> "hello_ack"
        | IntentMsg -> "intent"
        | MatchMsg -> "match"
        | NegotiateMsg -> "negotiate"
        | SettleMsg -> "settle"
        | QueryMsg -> "query"
        | ResponseMsg -> "response"
        | HeartbeatMsg -> "heartbeat"
        | GoodbyeMsg -> "goodbye"
      );
      "sender", `String msg.sender;
      "timestamp", `Float msg.timestamp;
      "payload", (
        match msg.payload with
        | Intent _intent -> `Null  (* TODO: implement intent serialization *)
        | Match _match_t -> `Null  (* TODO: implement match serialization *)
        | Settle _settlement -> `Null  (* TODO: implement settlement serialization *)
        | _ -> `Null  (* Simplified *)
      );
    ]
  
  (** Deserialize from JSON *)
  let from_json json =
    (* Simplified - would implement full deserialization *)
    None
  
  (** Serialize to binary *)
  let to_binary msg =
    let buffer = Buffer.create 1024 in
    
    (* Header: version (2 bytes) + type (1 byte) + flags (1 byte) *)
    Buffer.add_string buffer "AC";  (* Protocol identifier *)
    Buffer.add_char buffer (msg_type_to_byte msg.msg_type);
    Buffer.add_char buffer '\x00';  (* Flags *)
    
    (* Message ID (16 bytes) *)
    Buffer.add_string buffer (String.sub (msg.msg_id ^ String.make 16 '\x00') 0 16);
    
    (* Sender (32 bytes) *)
    Buffer.add_string buffer (String.sub (msg.sender ^ String.make 32 '\x00') 0 32);
    
    (* Timestamp (8 bytes) *)
    let timestamp_bytes = Bytes.create 8 in
    Bytes.set_int64_be timestamp_bytes 0 (Int64.of_float msg.timestamp);
    Buffer.add_bytes buffer timestamp_bytes;
    
    (* Payload (variable) - serialize to JSON for simplicity *)
    let payload_json = to_json msg |> Yojson.Safe.to_string in
    let payload_len = String.length payload_json in
    
    (* Payload length (4 bytes) *)
    let len_bytes = Bytes.create 4 in
    Bytes.set_int32_be len_bytes 0 (Int32.of_int payload_len);
    Buffer.add_bytes buffer len_bytes;
    
    (* Payload *)
    Buffer.add_string buffer payload_json;
    
    Buffer.to_bytes buffer
  
  (** Deserialize from binary *)
  let from_binary data =
    if Bytes.length data < 64 then
      None  (* Too short for header *)
    else
      (* Check protocol identifier *)
      let proto = Bytes.sub_string data 0 2 in
      if proto <> "AC" then
        None
      else
        (* Parse header *)
        let msg_type = byte_to_msg_type (Bytes.get data 2) in
        match msg_type with
        | None -> None
        | Some typ ->
            (* Extract fields *)
            let msg_id = Bytes.sub_string data 4 16 |> String.trim in
            let sender = Bytes.sub_string data 20 32 |> String.trim in
            let timestamp = Bytes.get_int64_be data 52 |> Int64.to_float in
            
            (* Get payload length *)
            let payload_len = Bytes.get_int32_be data 60 |> Int32.to_int in
            
            if Bytes.length data < 64 + payload_len then
              None  (* Incomplete message *)
            else
              (* For now, create a simple message *)
              Some {
                msg_id = msg_id;
                msg_type = typ;
                sender = sender;
                timestamp = timestamp;
                payload = Heartbeat { 
                  sequence = 0; 
                  load = 0.0; 
                  active_intents = 0;
                  active_matches = 0;
                };
                signature = None;
              }
end

(** {2 Message Creation} *)

(** Create hello message *)
let create_hello sender (info : agent_info) =
  {
    msg_id = Intent.generate_uuid ();
    msg_type = (Hello : msg_type);
    sender = sender;
    timestamp = Ambience_core.Time_provider.now ();
    payload = Hello {
      version = protocol_version;
      capabilities = info.capabilities;
      agent_info = info;
    };
    signature = None;
  }

(** Create intent message *)
let create_intent_msg sender intent =
  {
    msg_id = Intent.generate_uuid ();
    msg_type = IntentMsg;
    sender = sender;
    timestamp = Ambience_core.Time_provider.now ();
    payload = Intent intent;
    signature = None;
  }

(** Create match message *)
let create_match_msg sender match_t =
  {
    msg_id = Intent.generate_uuid ();
    msg_type = MatchMsg;
    sender = sender;
    timestamp = Ambience_core.Time_provider.now ();
    payload = Match match_t;
    signature = None;
  }

(** Create heartbeat message *)
let create_heartbeat sender seq load intents matches =
  {
    msg_id = Intent.generate_uuid ();
    msg_type = HeartbeatMsg;
    sender = sender;
    timestamp = Ambience_core.Time_provider.now ();
    payload = Heartbeat {
      sequence = seq;
      load = load;
      active_intents = intents;
      active_matches = matches;
    };
    signature = None;
  }

(** Create query message *)
let create_query sender query_type filters limit = {
  msg_id = Intent.generate_uuid ();
  msg_type = QueryMsg;
  sender = sender;
  timestamp = Ambience_core.Time_provider.now ();
  payload = Query {
    query_id = Intent.generate_uuid ();
    query_type = query_type;
    filters = filters;
    limit = limit;
  };
  signature = None;
}

(** Create goodbye message *)
let create_goodbye sender reason = {
  msg_id = Intent.generate_uuid ();
  msg_type = GoodbyeMsg;
  sender = sender;
  timestamp = Ambience_core.Time_provider.now ();
  payload = Goodbye reason;
  signature = None;
}

(** {2 Message Validation} *)

(** Validate message *)
let validate_message msg =
  (* Check timestamp is reasonable (not too old or future) *)
  let current = Ambience_core.Time_provider.now () in
  let age = abs_float (current -. msg.timestamp) in
  
  if age > 3600.0 then  (* More than 1 hour *)
    Error "Message timestamp out of range"
  else
    (* Validate payload *)
    match msg.payload with
    | Intent intent ->
        (* Validate intent *)
        if Intent.is_valid intent current then
          Ok ()
        else
          Error "Invalid intent"
    | _ ->
        Ok ()  (* Other validations *)

(** {2 Message Handling} *)

(** Message handler type *)
type handler = protocol_message -> unit

(** Message router *)
type router = {
  handlers: (msg_type, handler list) Hashtbl.t;
  default_handler: handler option;
}

(** Create message router *)
let create_router () = {
  handlers = Hashtbl.create 10;
  default_handler = None;
}

(** Register handler for message type *)
let register_handler router msg_type handler =
  let existing = 
    try Hashtbl.find router.handlers msg_type
    with Not_found -> []
  in
  Hashtbl.replace router.handlers msg_type (handler :: existing)

(** Route message to handlers *)
let route_message router msg =
  let handlers = 
    try Hashtbl.find router.handlers msg.msg_type
    with Not_found -> []
  in
  
  if handlers = [] then
    (* Use default handler if no specific handlers *)
    match router.default_handler with
    | None -> ()
    | Some handler -> handler msg
  else
    (* Call all registered handlers *)
    List.iter (fun handler -> handler msg) handlers

(** {2 Protocol Negotiation} *)

(** Protocol capabilities *)
type capability =
  | Compression     (* Supports compression *)
  | Encryption      (* Supports encryption *)
  | Multilateral    (* Supports multi-party matching *)
  | RateLimiting    (* Supports rate limiting *)
  | BinaryFormat    (* Supports binary format *)
  | JsonFormat      (* Supports JSON format *)

(** Capability to string *)
let capability_to_string = function
  | Compression -> "compression"
  | Encryption -> "encryption"
  | Multilateral -> "multilateral"
  | RateLimiting -> "rate_limiting"
  | BinaryFormat -> "binary"
  | JsonFormat -> "json"

(** String to capability *)
let string_to_capability = function
  | "compression" -> Some Compression
  | "encryption" -> Some Encryption
  | "multilateral" -> Some Multilateral
  | "rate_limiting" -> Some RateLimiting
  | "binary" -> Some BinaryFormat
  | "json" -> Some JsonFormat
  | _ -> None

(** Negotiate protocol version and capabilities *)
let negotiate_protocol our_version our_caps their_version their_caps =
  (* Check version compatibility *)
  if our_version <> their_version then
    Error "Version mismatch"
  else
    (* Find common capabilities *)
    let common_caps = 
      List.filter (fun cap ->
        List.mem cap their_caps
      ) our_caps
    in
    Ok common_caps

(** {2 Rate Limiting} *)

module RateLimit = struct
  type limiter = {
    max_messages_per_second: float;
    max_bytes_per_second: int;
    mutable message_count: float;
    mutable byte_count: int;
    mutable last_reset: float;
  }
  
  (** Create rate limiter *)
  let create ?(max_msgs = 100.0) ?(max_bytes = 1_000_000) () = {
    max_messages_per_second = max_msgs;
    max_bytes_per_second = max_bytes;
    message_count = 0.0;
    byte_count = 0;
    last_reset = Ambience_core.Time_provider.now ();
  }
  
  (** Check if message is allowed *)
  let check_message limiter size =
    let current = Ambience_core.Time_provider.now () in
    let elapsed = current -. limiter.last_reset in
    
    (* Reset if second has passed *)
    if elapsed >= 1.0 then begin
      limiter.message_count <- 0.0;
      limiter.byte_count <- 0;
      limiter.last_reset <- current
    end;
    
    (* Check limits *)
    if limiter.message_count >= limiter.max_messages_per_second then
      false
    else if limiter.byte_count + size > limiter.max_bytes_per_second then
      false
    else begin
      limiter.message_count <- limiter.message_count +. 1.0;
      limiter.byte_count <- limiter.byte_count + size;
      true
    end
end

(** {2 Compression} *)

module Compression = struct
  (** Compression algorithm *)
  type algorithm =
    | None
    | Gzip
    | Zstd
    | Lz4
  
  (** Compress data *)
  let compress algo data =
    match algo with
    | None -> data
    | _ -> data  (* TODO: Implement actual compression *)
  
  (** Decompress data *)
  let decompress algo data =
    match algo with
    | None -> data
    | _ -> data  (* TODO: Implement actual decompression *)
end