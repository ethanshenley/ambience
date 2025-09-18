(** Settlement Manifold Module
    
    This module handles the computation and analysis of settlement manifolds -
    the multi-dimensional spaces of possible settlements between intents.
    
    Key concepts:
    - Settlement spaces are continuous, not discrete
    - Multiple dimensions (price, quantity, time, quality)
    - Pareto optimality for multi-objective optimization
    - Constraint satisfaction in continuous spaces
*)

open Ambience_core.Types
open Ambience_core

(** Import Engine module for settlement space computation functions *)
module Engine = Engine

(** A point in n-dimensional settlement space *)
type point = {
  coordinates: (string * float) list;  (* dimension name -> value *)
  feasible: bool;                      (* Is this point valid? *)
  utility: (public_key * float) list;  (* Agent utilities at this point *)
}

(** Settlement space analyzer *)
type analyzer = {
  dimensions: string list;
  constraints: constraint_t list;
  sample_resolution: int;
  mutable points_evaluated: int;
  mutable feasible_region_size: float;
}

(** Create a new analyzer *)
let create_analyzer dimensions constraints ?(resolution = 100) () = {
  dimensions = dimensions;
  constraints = constraints;
  sample_resolution = resolution;
  points_evaluated = 0;
  feasible_region_size = 0.0;
}

(** {2 Dimension Management} *)

(** Standard dimensions in settlement spaces *)
module Dimensions = struct
  type dimension = {
    name: string;
    min_value: float;
    max_value: float;
    discrete: bool;           (* Is this dimension discrete? *)
    step_size: float option;  (* For discrete dimensions *)
  }
  
  (** Standard dimension: price *)
  let price_dimension min_price max_price = {
    name = "price";
    min_value = min_price;
    max_value = max_price;
    discrete = false;
    step_size = None;
  }
  
  (** Standard dimension: quantity *)
  let quantity_dimension min_qty max_qty ?(discrete = false) ?(step = 1.0) () = {
    name = "quantity";
    min_value = min_qty;
    max_value = max_qty;
    discrete = discrete;
    step_size = if discrete then Some step else None;
  }
  
  (** Standard dimension: time *)
  let time_dimension earliest latest = {
    name = "time";
    min_value = earliest;
    max_value = latest;
    discrete = false;
    step_size = None;
  }
  
  (** Standard dimension: quality *)
  let quality_dimension ?(min_quality = 0.0) ?(max_quality = 1.0) () = {
    name = "quality";
    min_value = min_quality;
    max_value = max_quality;
    discrete = false;
    step_size = None;
  }
  
  (** Discretize a continuous dimension *)
  let discretize dim n_steps =
    let range = dim.max_value -. dim.min_value in
    { dim with
      discrete = true;
      step_size = Some (range /. float_of_int n_steps)
    }
  
  (** Sample points along a dimension *)
  let sample_dimension dim n_samples =
    match dim.discrete, dim.step_size with
    | true, Some step ->
        (* Discrete sampling *)
        let rec sample current acc =
          if current > dim.max_value then acc
          else sample (current +. step) (current :: acc)
        in
        List.rev (sample dim.min_value [])
    | _ ->
        (* Continuous sampling *)
        List.init n_samples (fun i ->
          let t = float_of_int i /. float_of_int (n_samples - 1) in
          dim.min_value +. t *. (dim.max_value -. dim.min_value)
        )
end

(** {2 Constraint Evaluation} *)

(** Evaluate if a point satisfies constraints *)
let evaluate_constraints constraints point =
  let get_coord name = 
    List.assoc_opt name point.coordinates 
    |> Option.value ~default:0.0
  in
  
  List.for_all (fun constraint_ ->
    match constraint_ with
    | TimeWindow (start, end_) ->
        let time = get_coord "time" in
        time >= start && time <= end_
    | PriceRange (min_p, max_p) ->
        let price = get_coord "price" in
        price >= min_p && price <= max_p
    | _ -> true  (* Other constraints evaluated elsewhere *)
  ) constraints

(** {2 Utility Functions} *)

(** Calculate utility for an agent at a point *)
let calculate_utility intent point weights =
  let get_coord name = 
    List.assoc_opt name point.coordinates 
    |> Option.value ~default:0.0
  in
  
  (* Component utilities *)
  let price_utility =
    let price = get_coord "price" in
    match Intent.get_price_ranges intent with
    | [] -> 0.5  (* Neutral if no preference *)
    | (min_p, max_p) :: _ ->
        if price < min_p || price > max_p then 0.0
        else
          (* Prefer middle of acceptable range *)
          let mid = (min_p +. max_p) /. 2.0 in
          let deviation = abs_float (price -. mid) /. (max_p -. min_p) in
          1.0 -. (deviation *. 0.5)
  in
  
  let quantity_utility =
    let qty = get_coord "quantity" in
    let (min_q, max_q) = intent.want_field.quantity_range in
    if qty < min_q || qty > max_q then 0.0
    else (qty -. min_q) /. (max_q -. min_q)
  in
  
  let time_utility =
    let time = get_coord "time" in
    let current = Unix.time () in
    let delay = max 0.0 (time -. current) in
    exp (-. delay /. 3600.0)  (* Exponential decay - 1hr half-life *)
  in
  
  (* Weighted combination *)
  let (w_p, w_q, w_t) = weights in
  w_p *. price_utility +. w_q *. quantity_utility +. w_t *. time_utility

(** {2 Pareto Optimality} *)

(** Check if point p1 dominates p2 *)
let dominates p1 p2 =
  let all_better = ref true in
  let some_strictly_better = ref false in
  
  List.iter2 (fun (agent1, u1) (_agent2, u2) ->
    if u1 < u2 then
      all_better := false
    else if u1 > u2 then
      some_strictly_better := true
  ) p1.utility p2.utility;
  
  !all_better && !some_strictly_better

(** Find Pareto frontier from set of points *)
let find_pareto_frontier points =
  List.filter (fun p ->
    not (List.exists (fun q -> p != q && dominates q p) points)
  ) points

(** {2 Manifold Computation} *)

(** Sample the settlement space *)
let sample_space analyzer intent_a intent_b =
  (* Extract dimensions from intents *)
  let price_range = Engine.compute_price_range intent_a intent_b in
  let quantity_range = Engine.compute_quantity_range intent_a intent_b in
  let time_window = Engine.compute_time_window intent_a intent_b in
  
  match price_range, quantity_range, time_window with
  | Some (min_p, max_p), Some (min_q, max_q), Some (min_t, max_t) ->
      (* Create dimension objects *)
      let dims = [
        Dimensions.price_dimension min_p max_p;
        Dimensions.quantity_dimension min_q max_q ();
        Dimensions.time_dimension min_t max_t;
      ] in
      
      (* Generate grid of sample points *)
      let n = analyzer.sample_resolution in
      let points = ref [] in
      
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          for k = 0 to n - 1 do
            let t_i = float_of_int i /. float_of_int (n - 1) in
            let t_j = float_of_int j /. float_of_int (n - 1) in
            let t_k = float_of_int k /. float_of_int (n - 1) in
            
            let price = min_p +. t_i *. (max_p -. min_p) in
            let quantity = min_q +. t_j *. (max_q -. min_q) in
            let time = min_t +. t_k *. (max_t -. min_t) in
            
            let point = {
              coordinates = [
                ("price", price);
                ("quantity", quantity);
                ("time", time);
              ];
              feasible = true;  (* Will be evaluated *)
              utility = [];     (* Will be calculated *)
            } in
            
            analyzer.points_evaluated <- analyzer.points_evaluated + 1;
            points := point :: !points
          done
        done
      done;
      
      !points
  | _ -> []

(** Evaluate feasibility and utility for all points *)
let evaluate_points analyzer points (intent_a : intent) (intent_b : intent) =
  let weights = (0.4, 0.3, 0.3) in  (* Price, quantity, time weights *)
  
  List.map (fun point ->
    (* Check constraint satisfaction *)
    let feasible = 
      evaluate_constraints analyzer.constraints point &&
      evaluate_constraints intent_a.constraints point &&
      evaluate_constraints intent_b.constraints point
    in
    
    (* Calculate utilities if feasible *)
    let utility = 
      if feasible then [
        (intent_a.agent_id, calculate_utility intent_a point weights);
        (intent_b.agent_id, calculate_utility intent_b point weights);
      ] else []
    in
    
    { point with feasible = feasible; utility = utility }
  ) points

(** Compute complete manifold *)
let compute_manifold analyzer intent_a intent_b =
  (* Sample the space *)
  let sample_points = sample_space analyzer intent_a intent_b in
  
  (* Evaluate points *)
  let evaluated_points = evaluate_points analyzer sample_points intent_a intent_b in
  
  (* Filter to feasible points *)
  let feasible_points = List.filter (fun p -> p.feasible) evaluated_points in
  
  (* Calculate feasible region size *)
  analyzer.feasible_region_size <- 
    float_of_int (List.length feasible_points) /. 
    float_of_int (List.length evaluated_points);
  
  (* Find Pareto frontier *)
  let pareto_points = find_pareto_frontier feasible_points in
  
  (* Convert to settlement points *)
  let settlement_points = 
    List.map (fun p ->
      let price = List.assoc "price" p.coordinates in
      let quantity = List.assoc "quantity" p.coordinates in
      let time = List.assoc "time" p.coordinates in
      {
        price = price;
        quantity = quantity;
        execution_time = time;
        quality_level = None;
        additional_terms = [];
      }
    ) pareto_points
  in
  
  (* Create manifold *)
  {
    dimensions = analyzer.dimensions;
    valid_region_constraints = analyzer.constraints;
    pareto_frontier = settlement_points;
    optimality_scores = 
      Some (List.map (fun p ->
        let sp = {
          price = List.assoc "price" p.coordinates;
          quantity = List.assoc "quantity" p.coordinates;
          execution_time = List.assoc "time" p.coordinates;
          quality_level = None;
          additional_terms = [];
        } in
        let score = 
          List.fold_left (fun acc (_agent, u) -> acc +. u) 0.0 p.utility /.
          float_of_int (List.length p.utility)
        in
        (sp, score)
      ) pareto_points);
  }

(** {2 Manifold Analysis} *)

(** Calculate Nash bargaining solution *)
let nash_bargaining_solution manifold reservation_utilities =
  match manifold.optimality_scores with
  | None -> None
  | Some scores ->
      (* Find point that maximizes product of surplus utilities *)
      let nash_point =
        List.fold_left (fun best (point, _score) ->
          (* Calculate Nash product *)
          (* Simplified - in practice would need actual utilities *)
          match best with
          | None -> Some point
          | Some _best_point -> Some point  (* TODO: Actual comparison *)
        ) None scores
      in
      nash_point

(** Calculate Kalai-Smorodinsky solution *)
let kalai_smorodinsky_solution manifold =
  (* Find ideal point for each agent *)
  (* Then find feasible point closest to line from disagreement to ideal *)
  match manifold.pareto_frontier with
  | [] -> None
  | points -> Some (List.hd points)  (* Simplified *)

(** Manifold statistics *)
type manifold_stats = {
  dimensions: int;
  feasible_points: int;
  pareto_points: int;
  feasible_region_size: float;
  min_price: float option;
  max_price: float option;
  price_spread: float option;
}

(** Analyze manifold properties *)
let analyze_manifold manifold analyzer =
  let prices = 
    List.map (fun p -> p.price) manifold.pareto_frontier
  in
  
  let min_price = 
    match prices with [] -> None | ps -> Some (List.fold_left min infinity ps)
  in
  
  let max_price =
    match prices with [] -> None | ps -> Some (List.fold_left max neg_infinity ps)
  in
  
  let price_spread =
    match min_price, max_price with
    | Some min_p, Some max_p -> Some (max_p -. min_p)
    | _ -> None
  in
  
  {
    dimensions = List.length manifold.dimensions;
    feasible_points = analyzer.points_evaluated;
    pareto_points = List.length manifold.pareto_frontier;
    feasible_region_size = analyzer.feasible_region_size;
    min_price = min_price;
    max_price = max_price;
    price_spread = price_spread;
  }

(** {2 Visualization Support} *)

(** Project manifold to 2D for visualization *)
let project_to_2d manifold dim1 dim2 =
  List.map (fun point ->
    let coord1 = 
      match dim1 with
      | "price" -> point.price
      | "quantity" -> point.quantity
      | "time" -> point.execution_time
      | _ -> 0.0
    in
    let coord2 =
      match dim2 with
      | "price" -> point.price
      | "quantity" -> point.quantity
      | "time" -> point.execution_time
      | _ -> 0.0
    in
    (coord1, coord2)
  ) manifold.pareto_frontier

(** Export manifold to JSON for visualization *)
let to_json (manifold : settlement_manifold) =
  `Assoc [
    "dimensions", `List (List.map (fun d -> `String d) manifold.dimensions);
    "pareto_frontier", `List (
      List.map (fun p ->
        `Assoc [
          "price", `Float p.price;
          "quantity", `Float p.quantity;
          "time", `Float p.execution_time;
        ]
      ) manifold.pareto_frontier
    );
    "num_pareto_points", `Int (List.length manifold.pareto_frontier);
  ]