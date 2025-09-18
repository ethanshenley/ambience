(** Core module - entry point for the Ambient Commerce Protocol core
    
    This module re-exports all core components and provides initialization
    functions.
*)

(** {1 Core Modules} *)

module Types = Types
(** Core type definitions *)

module Intent = Intent  
(** Intent creation and management *)

module Resource = Resource
(** Resource ontology and transformations *)

module State = State
(** Global state management *)

(** {1 Initialization} *)

val init : unit -> State.t
(** Initialize the protocol and return initial state *)

(** {1 Version Information} *)

val version : string
(** Library version *)

val protocol_version : string
(** Protocol version string *)