(** Ambient Commerce Protocol - Main Entry Point *)

open Ambience_core.Types

(** {1 Configuration} *)

type node_config = {
  node_id: public_key;
  node_name: string;
  listen_port: int;
  bootstrap_nodes: string list;
  max_peers: int;
  matching_interval: float;
  enable_multilateral: bool;
  require_escrow: bool;
  reversal_window: float;
  min_reputation: float;
  require_collateral: bool;
  collateral_rate: float;
}

val default_config : public_key -> node_config

(** {1 Node Type} *)

type node

val create_node : node_config -> node

(** {1 Core Operations} *)

val post_intent :
  node ->
  offers:resource_field ->
  wants:resource_field ->
  ?constraints:constraint_t list ->
  ?lifecycle:lifecycle option ->
  unit -> (uuid, string) result

val execute_match : node -> match_t -> (uuid, string) result

val query_intents :
  node ->
  ?resource_type:resource_uri option ->
  ?max_results:int ->
  unit -> intent list

val get_node_stats : node -> Yojson.Safe.t

(** {1 Lifecycle} *)

val start : node -> (unit, string) result
val stop : node -> (unit, string) result

(** {1 Advanced} *)

module Advanced : sig
  val create_market_maker : node -> resource_uri -> float -> Thread.t
end