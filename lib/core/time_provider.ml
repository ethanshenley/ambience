(** Time Provider Module

    This module provides a configurable time abstraction for the entire protocol.
    It allows switching between real time (production) and mock time (testing),
    ensuring deterministic behavior in tests while using actual time in production.

    Design principles:
    - Single source of truth for time across all modules
    - Thread-safe time manipulation for testing
    - Zero overhead in production mode
    - Clear separation between real and mock time

    Usage:
    - In production: Uses Unix.time() directly
    - In tests: Can set fixed time or auto-advance
*)

(* Using float directly instead of importing Types to avoid circular dependency *)

(** Timestamp type - float representing Unix time *)
type timestamp = float

(** Time provider type - a function that returns current timestamp *)
type provider = unit -> timestamp

(** Time mode for clarity in configuration *)
type mode =
  | RealTime           (** Use system clock (Unix.time) *)
  | MockTime of float  (** Use fixed/controlled time for testing *)

(** Global time configuration - mutable for test control *)
module Config = struct
  (** The current time provider function *)
  let current_provider : provider ref = ref Unix.time

  (** Current mode for inspection *)
  let current_mode : mode ref = ref RealTime

  (** Auto-advance settings for mock time *)
  let auto_advance : float option ref = ref None

  (** Mutex for thread-safe time operations *)
  let time_mutex = Mutex.create ()
end

(** Get the current timestamp using the configured provider *)
let now () : timestamp =
  Mutex.lock Config.time_mutex;
  let time = !Config.current_provider () in
  (* Auto-advance mock time if configured *)
  (match !Config.current_mode, !Config.auto_advance with
   | MockTime current, Some advance ->
       let new_time = current +. advance in
       Config.current_mode := MockTime new_time;
       Config.current_provider := (fun () -> new_time)
   | _ -> ());
  Mutex.unlock Config.time_mutex;
  time

(** Set the time provider mode *)
let set_mode (mode : mode) : unit =
  Mutex.lock Config.time_mutex;
  Config.current_mode := mode;
  Config.current_provider :=
    (match mode with
     | RealTime -> Unix.time
     | MockTime fixed_time -> fun () -> fixed_time);
  Mutex.unlock Config.time_mutex

(** Use real system time (production mode) *)
let use_real_time () : unit =
  set_mode RealTime

(** Use mock time for testing *)
let use_mock_time (initial_time : float) : unit =
  set_mode (MockTime initial_time)

(** Get current time mode *)
let get_mode () : mode =
  !Config.current_mode

(** Set mock time to a specific value (only works in MockTime mode) *)
let set_time (time : float) : unit =
  match !Config.current_mode with
  | RealTime ->
      failwith "Cannot set time in RealTime mode. Use use_mock_time first."
  | MockTime _ ->
      Mutex.lock Config.time_mutex;
      Config.current_mode := MockTime time;
      Config.current_provider := (fun () -> time);
      Mutex.unlock Config.time_mutex

(** Advance mock time by a delta (only works in MockTime mode) *)
let advance_time (delta : float) : unit =
  match !Config.current_mode with
  | RealTime ->
      failwith "Cannot advance time in RealTime mode. Use use_mock_time first."
  | MockTime current ->
      set_time (current +. delta)

(** Configure auto-advance for mock time
    Each call to now() will advance time by this amount *)
let set_auto_advance (delta : float option) : unit =
  Config.auto_advance := delta

(** Reset to production defaults *)
let reset () : unit =
  use_real_time ();
  Config.auto_advance := None

(** Time utilities *)
module Utils = struct
  (** Get current timestamp (alias for now) *)
  let current_timestamp () : timestamp = now ()

  (** Check if a timestamp is in the past *)
  let is_past (time : timestamp) : bool =
    time < now ()

  (** Check if a timestamp is in the future *)
  let is_future (time : timestamp) : bool =
    time > now ()

  (** Time until a future timestamp (returns 0 if in past) *)
  let time_until (future : timestamp) : float =
    max 0.0 (future -. now ())

  (** Time since a past timestamp (returns 0 if in future) *)
  let time_since (past : timestamp) : float =
    max 0.0 (now () -. past)

  (** Check if current time is within a window *)
  let in_window (start_t : timestamp) (end_t : timestamp) : bool =
    let current = now () in
    current >= start_t && current <= end_t

  (** Format timestamp as ISO 8601 string *)
  let to_iso8601 (time : timestamp) : string =
    let tm = Unix.gmtime time in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (1900 + tm.Unix.tm_year)
      (1 + tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec

  (** Parse ISO 8601 string to timestamp (simplified) *)
  let from_iso8601 (str : string) : timestamp option =
    try
      Scanf.sscanf str "%d-%d-%dT%d:%d:%dZ"
        (fun year month day hour min sec ->
          let tm = {
            Unix.tm_year = year - 1900;
            tm_mon = month - 1;
            tm_mday = day;
            tm_hour = hour;
            tm_min = min;
            tm_sec = sec;
            tm_wday = 0;  (* Will be computed *)
            tm_yday = 0;  (* Will be computed *)
            tm_isdst = false;
          } in
          Some (Unix.mktime tm |> fst))
    with _ -> None
end

(** Testing utilities - only available in test configurations *)
module Test = struct
  (** Save current time configuration *)
  type saved_config = {
    mode: mode;
    auto_advance: float option;
  }

  (** Save current configuration (for test isolation) *)
  let save_config () : saved_config =
    { mode = !Config.current_mode;
      auto_advance = !Config.auto_advance }

  (** Restore saved configuration *)
  let restore_config (config : saved_config) : unit =
    set_mode config.mode;
    Config.auto_advance := config.auto_advance

  (** Run a function with mock time, then restore previous config *)
  let with_mock_time (time : float) (f : unit -> 'a) : 'a =
    let saved = save_config () in
    use_mock_time time;
    let result =
      try f ()
      with e ->
        restore_config saved;
        raise e
    in
    restore_config saved;
    result

  (** Run a function with auto-advancing mock time *)
  let with_auto_advance (initial : float) (advance : float) (f : unit -> 'a) : 'a =
    let saved = save_config () in
    use_mock_time initial;
    set_auto_advance (Some advance);
    let result =
      try f ()
      with e ->
        restore_config saved;
        raise e
    in
    restore_config saved;
    result
end