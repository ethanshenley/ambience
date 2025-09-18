(** Resource management module
    
    Handles resource definitions, transformations, and the resource ontology.
*)

open Types

(** {1 Resource Ontology} *)

module Ontology : sig
  type node = {
    uri: resource_uri;
    name: string;
    description: string;
    parent: resource_uri option;
    attributes: (string * string) list;
    fungible: bool;
    base_unit: string;
  }
  
  val exists : resource_uri -> bool
  (** Check if a resource type exists in the ontology *)
  
  val get_metadata : resource_uri -> node option
  (** Get metadata for a resource type *)
  
  val is_subtype_of : resource_uri -> resource_uri -> bool
  (** Check if one resource is a subtype of another *)
  
  val get_ancestors : resource_uri -> resource_uri list
  (** Get all parent types of a resource *)
  
  val are_exchangeable : resource_uri -> resource_uri -> bool
  (** Check if two resources can be exchanged *)
end

(** {1 Resource Field Operations} *)

val create_field :
  resource_type:resource_uri ->
  min_quantity:float ->
  max_quantity:float ->
  quality:quality ->
  metadata:(string * string) list ->
  (resource_field, string) result
(** Create a resource field with validation *)

val fields_compatible : resource_field -> resource_field -> bool
(** Check if two resource fields can potentially match *)

val calculate_tradeable_range : 
  resource_field -> resource_field -> (float * float) option
(** Calculate the actual tradeable quantity range between two fields *)

(** {1 Resource Transformations} *)

module Transformations : sig
  type transformation = {
    from_resource: resource_uri;
    to_resource: resource_uri;
    rate: float;
    conditions: (string * string) list;
    reversible: bool;
  }
  
  val register :
    from_resource:resource_uri ->
    to_resource:resource_uri ->
    rate:float ->
    ?conditions:(string * string) list ->
    ?reversible:bool ->
    unit -> unit
  (** Register a transformation between resources *)
  
  val find_transformation_path : 
    resource_uri -> resource_uri -> (transformation list * float) option
  (** Find transformation path between two resources *)
end

val calculate_exchange_rate : resource_uri -> resource_uri -> float option
(** Calculate exchange rate between two resources *)

(** {1 Quantity and Units} *)

type quantity = {
  amount: float;
  unit: string;
  resource_type: resource_uri;
}

val convert_units : quantity -> to_unit:string -> quantity
(** Convert between different units *)

(** {1 Statistics} *)

type resource_stats = {
  total_volume: float;
  transaction_count: int;
  average_price: float option;
  price_range: (float * float) option;
}

val calculate_stats : settlement list -> resource_uri -> resource_stats
(** Calculate statistics from settlements *)

(** {1 Utility Functions} *)

val validate_uri : resource_uri -> (unit, string) result
(** Validate a resource URI *)

val field_to_string : resource_field -> string
(** Pretty print a resource field *)