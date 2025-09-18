(** Time Provider Module - Configurable time abstraction for the protocol

    This module provides a unified time interface that can switch between
    real time (production) and mock time (testing), ensuring consistent
    time handling across the entire protocol.
*)

(** {1 Types} *)

(** Timestamp type - float representing Unix time *)
type timestamp = float

(** Time provider function type *)
type provider = unit -> timestamp

(** Time mode configuration *)
type mode =
  | RealTime           (** Use system clock (Unix.time) *)
  | MockTime of float  (** Use fixed/controlled time for testing *)

(** {1 Core Functions} *)

(** Get the current timestamp using the configured provider.
    This is the primary function all modules should use for getting time. *)
val now : unit -> timestamp

(** {1 Configuration} *)

(** Set the time provider mode *)
val set_mode : mode -> unit

(** Use real system time (production mode) *)
val use_real_time : unit -> unit

(** Use mock time for testing with initial timestamp *)
val use_mock_time : float -> unit

(** Get current time mode *)
val get_mode : unit -> mode

(** Reset to production defaults (real time, no auto-advance) *)
val reset : unit -> unit

(** {1 Mock Time Control (Testing Only)} *)

(** Set mock time to a specific value.
    @raise Failure if called in RealTime mode *)
val set_time : float -> unit

(** Advance mock time by a delta.
    @raise Failure if called in RealTime mode *)
val advance_time : float -> unit

(** Configure auto-advance for mock time.
    When set, each call to [now()] will advance time by this amount.
    Pass [None] to disable auto-advance. *)
val set_auto_advance : float option -> unit

(** {1 Utilities} *)

module Utils : sig
  (** Get current timestamp (alias for now) *)
  val current_timestamp : unit -> timestamp

  (** Check if a timestamp is in the past *)
  val is_past : timestamp -> bool

  (** Check if a timestamp is in the future *)
  val is_future : timestamp -> bool

  (** Time until a future timestamp (returns 0 if in past) *)
  val time_until : timestamp -> float

  (** Time since a past timestamp (returns 0 if in future) *)
  val time_since : timestamp -> float

  (** Check if current time is within a window *)
  val in_window : timestamp -> timestamp -> bool

  (** Format timestamp as ISO 8601 string *)
  val to_iso8601 : timestamp -> string

  (** Parse ISO 8601 string to timestamp *)
  val from_iso8601 : string -> timestamp option
end

(** {1 Testing Utilities} *)

module Test : sig
  (** Saved time configuration for test isolation *)
  type saved_config

  (** Save current time configuration *)
  val save_config : unit -> saved_config

  (** Restore saved time configuration *)
  val restore_config : saved_config -> unit

  (** Run a function with mock time, then restore previous config *)
  val with_mock_time : float -> (unit -> 'a) -> 'a

  (** Run a function with auto-advancing mock time *)
  val with_auto_advance : float -> float -> (unit -> 'a) -> 'a
end