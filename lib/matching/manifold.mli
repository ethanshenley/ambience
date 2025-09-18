(** Settlement Manifold Module
    
    Computation and analysis of multi-dimensional settlement spaces.
*)

open Ambience_core.Types

(** {1 Core Types} *)

type point = {
  coordinates: (string * float) list;
  feasible: bool;
  utility: (public_key * float) list;
}
(** A point in settlement space *)

type analyzer
(** Settlement space analyzer *)

val create_analyzer : 
  string list -> constraint_t list -> ?resolution:int -> unit -> analyzer
(** Create analyzer with dimensions and constraints *)

(** {1 Dimensions} *)

module Dimensions : sig
  type dimension = {
    name: string;
    min_value: float;
    max_value: float;
    discrete: bool;
    step_size: float option;
  }
  
  val price_dimension : float -> float -> dimension
  val quantity_dimension : 
    float -> float -> ?discrete:bool -> ?step:float -> unit -> dimension
  val time_dimension : float -> float -> dimension
  val quality_dimension : ?min_quality:float -> ?max_quality:float -> unit -> dimension
  
  val discretize : dimension -> int -> dimension
  val sample_dimension : dimension -> int -> float list
end

(** {1 Manifold Computation} *)

val compute_manifold : analyzer -> intent -> intent -> settlement_manifold
(** Compute complete settlement manifold between two intents *)

val find_pareto_frontier : point list -> point list
(** Find Pareto-optimal points from a set *)

(** {1 Solution Concepts} *)

val nash_bargaining_solution : 
  settlement_manifold -> (public_key * float) list -> settlement_point option
(** Calculate Nash bargaining solution *)

val kalai_smorodinsky_solution : settlement_manifold -> settlement_point option
(** Calculate Kalai-Smorodinsky solution *)

(** {1 Analysis} *)

type manifold_stats = {
  dimensions: int;
  feasible_points: int;
  pareto_points: int;
  feasible_region_size: float;
  min_price: float option;
  max_price: float option;
  price_spread: float option;
}

val analyze_manifold : settlement_manifold -> analyzer -> manifold_stats
(** Analyze manifold properties *)

(** {1 Visualization} *)

val project_to_2d : settlement_manifold -> string -> string -> (float * float) list
(** Project manifold to 2D for visualization *)

val to_json : settlement_manifold -> Yojson.Safe.t
(** Export manifold to JSON *)