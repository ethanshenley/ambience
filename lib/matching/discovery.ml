(** Match Discovery Module
    
    This module handles the discovery of potential matches between intents.
    It implements various discovery strategies and optimizations for finding
    compatible intent combinations efficiently.
    
    Key concepts:
    - Discovery strategies (exhaustive, indexed, probabilistic)
    - Intent indexing for fast lookup
    - Compatibility caching
    - Multi-hop path discovery
*)

open Ambience_core.Types
open Ambience_core

(** Discovery strategy *)
type strategy = 
  | Exhaustive              (* Check all pairs - O(n²) *)
  | Indexed                  (* Use resource-based indexing *)
  | Probabilistic of float   (* Sample random subset *)
  | Hybrid                   (* Combine strategies based on load *)

(** Discovery context *)
type context = {
  strategy: strategy;
  max_path_length: int;     (* For multi-hop trades *)
  cache_enabled: bool;
  compatibility_cache: (string * string, bool) Hashtbl.t;
  mutable cache_hits: int;
  mutable cache_misses: int;
}

(** Create a new discovery context *)
let create_context ?(strategy = Indexed) ?(max_path_length = 3) ?(cache_enabled = true) () = {
  strategy = strategy;
  max_path_length = max_path_length;
  cache_enabled = cache_enabled;
  compatibility_cache = Hashtbl.create 10000;
  cache_hits = 0;
  cache_misses = 0;
}

(** {2 Compatibility Checking with Caching} *)

(** Generate cache key for two intents *)
let cache_key intent_a intent_b =
  (* Order intent IDs to ensure consistent keys *)
  if intent_a.intent_id < intent_b.intent_id then
    (intent_a.intent_id, intent_b.intent_id)
  else
    (intent_b.intent_id, intent_a.intent_id)

(** Check compatibility with caching *)
let check_compatibility context intent_a intent_b =
  if not context.cache_enabled then
    Intent.are_compatible intent_a intent_b
  else
    let key = cache_key intent_a intent_b in
    try
      let result = Hashtbl.find context.compatibility_cache key in
      context.cache_hits <- context.cache_hits + 1;
      result
    with Not_found ->
      context.cache_misses <- context.cache_misses + 1;
      let result = Intent.are_compatible intent_a intent_b in
      Hashtbl.add context.compatibility_cache key result;
      result

(** Clear compatibility cache *)
let clear_cache context =
  Hashtbl.clear context.compatibility_cache;
  context.cache_hits <- 0;
  context.cache_misses <- 0

(** {2 Intent Indexing} *)

(** Intent index for efficient discovery *)
module Index = struct
  type t = {
    (* Index by offered resource *)
    by_offer: (resource_uri, intent list) Hashtbl.t;
    (* Index by wanted resource *)
    by_want: (resource_uri, intent list) Hashtbl.t;
    (* Index by agent *)
    by_agent: (public_key, intent list) Hashtbl.t;
    (* Index by time window *)
    by_time: (int, intent list) Hashtbl.t;  (* Bucketed by hour *)
  }
  
  (** Create empty index *)
  let create () = {
    by_offer = Hashtbl.create 100;
    by_want = Hashtbl.create 100;
    by_agent = Hashtbl.create 1000;
    by_time = Hashtbl.create 24;
  }
  
  (** Add intent to index *)
  let add index intent =
    (* Index by offer *)
    let offer_list = 
      try Hashtbl.find index.by_offer intent.offer_field.resource_type
      with Not_found -> []
    in
    Hashtbl.replace index.by_offer intent.offer_field.resource_type 
      (intent :: offer_list);
    
    (* Index by want *)
    let want_list = 
      try Hashtbl.find index.by_want intent.want_field.resource_type
      with Not_found -> []
    in
    Hashtbl.replace index.by_want intent.want_field.resource_type 
      (intent :: want_list);
    
    (* Index by agent *)
    let agent_list = 
      try Hashtbl.find index.by_agent intent.agent_id
      with Not_found -> []
    in
    Hashtbl.replace index.by_agent intent.agent_id (intent :: agent_list);
    
    (* Index by time - bucket into hours *)
    List.iter (fun (start_time, _end_time) ->
      let hour = int_of_float (start_time /. 3600.0) mod 24 in
      let time_list = 
        try Hashtbl.find index.by_time hour
        with Not_found -> []
      in
      Hashtbl.replace index.by_time hour (intent :: time_list)
    ) (Intent.get_time_windows intent)
  
  (** Find potential matches for an intent using index *)
  let find_candidates index intent =
    (* Look for intents that offer what this intent wants *)
    let candidates = 
      try Hashtbl.find index.by_offer intent.want_field.resource_type
      with Not_found -> []
    in
    
    (* Filter to those that also want what this intent offers *)
    List.filter (fun candidate ->
      candidate.want_field.resource_type = intent.offer_field.resource_type
    ) candidates
  
  (** Build index from intent list *)
  let build intents =
    let index = create () in
    List.iter (add index) intents;
    index
end

(** {2 Discovery Strategies} *)

(** Exhaustive search - check all pairs *)
let discover_exhaustive context intents =
  let matches = ref [] in
  let n = List.length intents in
  let intent_array = Array.of_list intents in
  
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let intent_a = intent_array.(i) in
      let intent_b = intent_array.(j) in
      
      if check_compatibility context intent_a intent_b then
        matches := (intent_a, intent_b) :: !matches
    done
  done;
  
  !matches

(** Indexed search - use resource index *)
let discover_indexed context intents =
  let index = Index.build intents in
  let matches = ref [] in
  let seen_pairs = Hashtbl.create 1000 in
  
  List.iter (fun intent ->
    let candidates = Index.find_candidates index intent in
    
    List.iter (fun candidate ->
      (* Avoid duplicate pairs *)
      let key = cache_key intent candidate in
      if not (Hashtbl.mem seen_pairs key) then begin
        Hashtbl.add seen_pairs key true;
        
        if check_compatibility context intent candidate then
          matches := (intent, candidate) :: !matches
      end
    ) candidates
  ) intents;
  
  !matches

(** Probabilistic search - sample random subset *)
let discover_probabilistic context sample_rate intents =
  let n = List.length intents in
  let sample_size = int_of_float (float_of_int n *. sample_rate) in
  
  (* Random sample of intents *)
  let sampled = 
    intents
    |> List.map (fun i -> (Random.float 1.0, i))
    |> List.sort (fun (r1, _) (r2, _) -> Float.compare r1 r2)
    |> List.map snd
    |> List.filteri (fun idx _ -> idx < sample_size)
  in
  
  (* Run exhaustive on sample *)
  discover_exhaustive context sampled

(** Hybrid strategy - adapt based on load *)
let discover_hybrid context intents =
  let n = List.length intents in
  
  if n < 100 then
    (* Small set - use exhaustive *)
    discover_exhaustive context intents
  else if n < 1000 then
    (* Medium set - use indexed *)
    discover_indexed context intents
  else
    (* Large set - use probabilistic sampling then indexed *)
    let sample_rate = min 1.0 (1000.0 /. float_of_int n) in
    discover_probabilistic context sample_rate intents

(** Main discovery function *)
let discover_pairs context intents =
  match context.strategy with
  | Exhaustive -> discover_exhaustive context intents
  | Indexed -> discover_indexed context intents
  | Probabilistic rate -> discover_probabilistic context rate intents
  | Hybrid -> discover_hybrid context intents

(** {2 Multi-hop Discovery} *)

(** Path in a multi-hop trade *)
type trade_path = {
  intents: intent list;
  resources: resource_uri list;  (* Resources flowing through path *)
  cycle: bool;                   (* Is this a circular trade? *)
}

(** Find circular trade paths (A->B->C->A) *)
let find_circular_paths _context intents max_length =
  let paths = ref [] in
  
  (* Build graph of who wants what *)
  let wants_graph = Hashtbl.create 100 in
  let offers_graph = Hashtbl.create 100 in
  
  List.iter (fun intent ->
    (* Add to wants graph *)
    let wants_list = 
      try Hashtbl.find wants_graph intent.want_field.resource_type
      with Not_found -> []
    in
    Hashtbl.replace wants_graph intent.want_field.resource_type 
      (intent :: wants_list);
    
    (* Add to offers graph *)
    let offers_list = 
      try Hashtbl.find offers_graph intent.offer_field.resource_type
      with Not_found -> []
    in
    Hashtbl.replace offers_graph intent.offer_field.resource_type 
      (intent :: offers_list)
  ) intents;
  
  (* DFS to find cycles *)
  let rec find_cycle current_intent path visited =
    if List.length path >= max_length then
      None
    else
      let next_resource = current_intent.want_field.resource_type in
      
      (* Find who offers what current intent wants *)
      let candidates = 
        try Hashtbl.find offers_graph next_resource
        with Not_found -> []
      in
      
      (* Check each candidate *)
      List.find_map (fun candidate ->
        if List.mem candidate.intent_id visited then
          (* Check if this completes a cycle *)
          if List.length path > 2 && 
             candidate.intent_id = (List.hd path).intent_id then
            (* Found a cycle! *)
            Some {
              intents = List.rev (current_intent :: path);
              resources = List.map (fun i -> i.offer_field.resource_type) 
                          (List.rev (current_intent :: path));
              cycle = true;
            }
          else
            None
        else
          (* Continue searching *)
          find_cycle candidate 
            (current_intent :: path) 
            (candidate.intent_id :: visited)
      ) candidates
  in
  
  (* Try starting from each intent *)
  List.iter (fun start_intent ->
    match find_cycle start_intent [start_intent] [start_intent.intent_id] with
    | Some path -> paths := path :: !paths
    | None -> ()
  ) intents;
  
  !paths

(** {2 Discovery Optimization} *)

(** Pre-filter intents before discovery *)
let prefilter_intents intents =
  let current_time = Ambience_core.Time_provider.now () in
  
  intents
  |> List.filter (fun i -> Intent.is_valid i current_time)
  |> List.filter (fun i -> Intent.constraints_satisfied i current_time)
  |> Intent.Batch.sort_by_priority

(** Partition intents by resource for parallel discovery *)
let partition_by_resource intents =
  let partitions = Hashtbl.create 10 in
  
  List.iter (fun intent ->
    let key = intent.offer_field.resource_type in
    let partition = 
      try Hashtbl.find partitions key
      with Not_found -> []
    in
    Hashtbl.replace partitions key (intent :: partition)
  ) intents;
  
  Hashtbl.fold (fun _key intents acc -> intents :: acc) partitions []

(** Discovery statistics *)
type stats = {
  total_intents: int;
  pairs_checked: int;
  matches_found: int;
  cache_hit_rate: float;
  discovery_time: float;
}

(** Run discovery with statistics *)
let discover_with_stats context intents =
  let start_time = Ambience_core.Time_provider.now () in
  
  (* Reset cache stats *)
  context.cache_hits <- 0;
  context.cache_misses <- 0;
  
  (* Prefilter *)
  let filtered = prefilter_intents intents in
  
  (* Discover pairs *)
  let pairs = discover_pairs context filtered in
  
  let elapsed = Ambience_core.Time_provider.now () -. start_time in
  
  let stats = {
    total_intents = List.length intents;
    pairs_checked = context.cache_hits + context.cache_misses;
    matches_found = List.length pairs;
    cache_hit_rate = 
      if context.cache_hits + context.cache_misses > 0 then
        float_of_int context.cache_hits /. 
        float_of_int (context.cache_hits + context.cache_misses)
      else 0.0;
    discovery_time = elapsed;
  } in
  
  (pairs, stats)