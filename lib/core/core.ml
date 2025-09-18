(** Core module - re-exports all core components
    
    This module provides a single entry point to all core protocol types
    and functions.
*)

(** Core type definitions *)
module Types = Types

(** Intent management *)
module Intent = Intent

(** Resource management *)
module Resource = Resource

(** State management *)
module State = State

(** Convenience function to initialize everything *)
let init () =
  (* Resource ontology is initialized on module load *)
  (* Create initial protocol state *)
  State.create ()

(** Version information *)
let version = "0.1.0"
let protocol_version = "ACP/0.1"