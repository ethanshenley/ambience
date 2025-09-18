(** Network Protocol Module
    
    Wire protocol for the Ambient Commerce Protocol.
*)

open Ambience_core.Types

(** {1 Protocol Version} *)

val protocol_version : string

(** {1 Message Types} *)

type msg_type =
  | Hello
  | HelloAck
  | IntentMsg
  | MatchMsg
  | NegotiateMsg
  | SettleMsg
  | QueryMsg
  | ResponseMsg
  | HeartbeatMsg
  | GoodbyeMsg

type protocol_message

type agent_info = {
  agent_id: public_key;
  name: string option;
  capabilities: string list;
  reputation: float;
  metadata: (string * string) list;
}

(** {1 Message Creation} *)

val create_hello : public_key -> agent_info -> protocol_message
val create_intent_msg : public_key -> intent -> protocol_message
val create_match_msg : public_key -> match_t -> protocol_message
val create_heartbeat : public_key -> int -> float -> int -> int -> protocol_message

(** {1 Serialization} *)

module Serialization : sig
  val to_json : protocol_message -> Yojson.Safe.t
  val from_json : Yojson.Safe.t -> protocol_message option
  val to_binary : protocol_message -> bytes
  val from_binary : bytes -> protocol_message option
end

(** {1 Message Validation} *)

val validate_message : protocol_message -> (unit, string) result

(** {1 Message Routing} *)

type handler = protocol_message -> unit
type router

val create_router : unit -> router
val register_handler : router -> msg_type -> handler -> unit
val route_message : router -> protocol_message -> unit

(** {1 Protocol Capabilities} *)

type capability =
  | Compression
  | Encryption
  | Multilateral
  | RateLimiting
  | BinaryFormat
  | JsonFormat

val capability_to_string : capability -> string
val string_to_capability : string -> capability option
val negotiate_protocol : 
  string -> capability list -> string -> capability list -> 
  (capability list, string) result

(** {1 Rate Limiting} *)

module RateLimit : sig
  type limiter
  val create : ?max_msgs:float -> ?max_bytes:int -> unit -> limiter
  val check_message : limiter -> int -> bool
end

(** {1 Compression} *)

module Compression : sig
  type algorithm = None | Gzip | Zstd | Lz4
  val compress : algorithm -> bytes -> bytes
  val decompress : algorithm -> bytes -> bytes
end