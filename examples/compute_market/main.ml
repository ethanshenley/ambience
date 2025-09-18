(** GPU Compute Marketplace Example
    
    This example shows how to build a decentralized GPU compute marketplace
    where providers can rent out their GPUs and consumers can purchase compute time.
*)

open Ambient_core
open Ambient

(** GPU provider agent *)
let run_gpu_provider node gpu_type hourly_rate =
  Printf.printf "Starting GPU provider for %s at $%.2f/hour\n" gpu_type hourly_rate;
  
  (* Create GPU resource *)
  let gpu_resource = 
    Resource.create_field
      ~resource_type:("compute:gpu:" ^ gpu_type)
      ~min_quantity:1.0
      ~max_quantity:24.0  (* Max 24 hours *)
      ~quality:Types.Fungible
      ~metadata:[
        "cuda_version", "12.0";
        "memory", "24GB";
        "availability", "immediate";
      ]
    |> Result.get_ok
  in
  
  (* Create price requirement *)
  let price_requirement = 
    Resource.create_field
      ~resource_type:"currency:fiat:USD"
      ~min_quantity:(hourly_rate *. 1.0)
      ~max_quantity:(hourly_rate *. 24.0)
      ~quality:Types.Fungible
      ~metadata:[]
    |> Result.get_ok
  in
  
  (* Post intent *)
  match post_intent node ~offers:gpu_resource ~wants:price_requirement
          ~constraints:[
            Types.TimeWindow (Unix.time (), Unix.time () +. 86400.0);
          ] () with
  | Error e -> Printf.printf "Failed to post GPU offer: %s\n" e
  | Ok intent_id -> Printf.printf "Posted GPU offer with ID: %s\n" intent_id

(** GPU consumer agent *)
let run_gpu_consumer node gpu_type hours max_price =
  Printf.printf "Looking for %s GPU for %.1f hours (max $%.2f/hour)\n" 
    gpu_type hours max_price;
  
  (* Create compute requirement *)
  let compute_need = 
    Resource.create_field
      ~resource_type:("compute:gpu:" ^ gpu_type)
      ~min_quantity:hours
      ~max_quantity:hours
      ~quality:Types.Fungible
      ~metadata:[
        "purpose", "model_training";
        "framework", "pytorch";
      ]
    |> Result.get_ok
  in
  
  (* Create payment offer *)
  let payment_offer = 
    Resource.create_field
      ~resource_type:"currency:fiat:USD"
      ~min_quantity:(hours *. 1.0)
      ~max_quantity:(hours *. max_price)
      ~quality:Types.Fungible
      ~metadata:[]
    |> Result.get_ok
  in
  
  (* Post intent *)
  match post_intent node ~offers:payment_offer ~wants:compute_need
          ~constraints:[
            Types.TimeWindow (Unix.time (), Unix.time () +. 3600.0);  (* Need within 1 hour *)
          ] () with
  | Error e -> Printf.printf "Failed to post compute request: %s\n" e
  | Ok intent_id -> 
      Printf.printf "Posted compute request with ID: %s\n" intent_id;
      
      (* Wait for match *)
      let rec check_for_match () =
        Unix.sleep 1;
        let matches = State.Queries.get_intent_matches node.state intent_id in
        match matches with
        | [] -> 
            Printf.printf ".";
            flush stdout;
            check_for_match ()
        | match_t :: _ ->
            Printf.printf "\nFound match! Executing settlement...\n";
            match execute_match node match_t with
            | Error e -> Printf.printf "Settlement failed: %s\n" e
            | Ok settlement_id -> Printf.printf "Settlement executed: %s\n" settlement_id
      in
      check_for_match ()

(** Market statistics monitor *)
let monitor_market node =
  let rec loop () =
    let stats = get_node_stats node in
    Printf.printf "\n=== Market Statistics ===\n";
    Printf.printf "%s\n" (Yojson.Safe.pretty_to_string stats);
    Unix.sleep 10;
    loop ()
  in
  Thread.create loop ()

(** Main entry point *)
let () =
  (* Create node *)
  let node_id = "gpu_market_node_" ^ string_of_int (Random.int 1000) in
  let config = {
    (default_config node_id) with
    bootstrap_nodes = ["tcp://localhost:8545"; "tcp://localhost:8546"];
  } in
  let node = create_node config in
  
  (* Start node *)
  let _ = start node in
  Printf.printf "Started GPU marketplace node: %s\n" node_id;
  
  (* Parse command line arguments *)
  match Array.to_list Sys.argv with
  | _ :: "provider" :: gpu_type :: rate :: _ ->
      run_gpu_provider node gpu_type (float_of_string rate)
  
  | _ :: "consumer" :: gpu_type :: hours :: max_price :: _ ->
      run_gpu_consumer node gpu_type (float_of_string hours) (float_of_string max_price)
  
  | _ :: "market-maker" :: _ ->
      Printf.printf "Starting market maker for GPU compute\n";
      let _ = Advanced.create_market_maker node "compute:gpu:nvidia:4090" 0.05 in
      let _ = monitor_market node in
      (* Keep running *)
      while true do Unix.sleep 1 done
  
  | _ ->
      Printf.printf "Usage:\n";
      Printf.printf "  %s provider <gpu_type> <hourly_rate>\n" Sys.argv.(0);
      Printf.printf "  %s consumer <gpu_type> <hours> <max_price_per_hour>\n" Sys.argv.(0);
      Printf.printf "  %s market-maker\n" Sys.argv.(0);
      Printf.printf "\nExample:\n";
      Printf.printf "  %s provider nvidia:4090 5.0\n" Sys.argv.(0);
      Printf.printf "  %s consumer nvidia:4090 2.0 6.0\n" Sys.argv.(0)