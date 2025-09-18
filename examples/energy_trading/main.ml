(** Decentralized Energy Trading Example
    
    This example demonstrates peer-to-peer energy trading where solar panel
    owners can sell excess energy to their neighbors.
*)

open Ambient_core
open Ambient

(** Energy producer (solar panel owner) *)
let run_energy_producer node kwh_available price_per_kwh =
  Printf.printf "Offering %.1f kWh at $%.3f per kWh\n" kwh_available price_per_kwh;
  
  (* Create energy offer *)
  let energy = 
    Resource.create_field
      ~resource_type:"energy:electricity:solar"
      ~min_quantity:1.0
      ~max_quantity:kwh_available
      ~quality:Types.Fungible
      ~metadata:[
        "source", "rooftop_solar";
        "voltage", "240V";
        "phase", "single";
        "availability", "immediate";
      ]
    |> Result.get_ok
  in
  
  (* Create price *)
  let price = 
    Resource.create_field
      ~resource_type:"currency:fiat:USD"
      ~min_quantity:price_per_kwh
      ~max_quantity:(price_per_kwh *. kwh_available)
      ~quality:Types.Fungible
      ~metadata:[]
    |> Result.get_ok
  in
  
  (* Post intent with time window (energy must be used soon) *)
  match post_intent node ~offers:energy ~wants:price
          ~constraints:[
            Types.TimeWindow (Unix.time (), Unix.time () +. 3600.0);  (* Next hour *)
          ]
          ~lifecycle:(Some (Types.Expiring (Unix.time () +. 1800.0))) (* Expires in 30 min *)
          () with
  | Error e -> Printf.printf "Failed to post energy offer: %s\n" e
  | Ok intent_id -> 
      Printf.printf "Posted energy offer: %s\n" intent_id;
      
      (* Monitor for matches *)
      let rec monitor () =
        Unix.sleep 2;
        let matches = State.Queries.get_intent_matches node.state intent_id in
        List.iter (fun match_t ->
          Printf.printf "Matched! Selling energy...\n";
          let _ = execute_match node match_t in
          ()
        ) matches;
        monitor ()
      in
      monitor ()

(** Energy consumer *)
let run_energy_consumer node kwh_needed max_price =
  Printf.printf "Looking for %.1f kWh (max $%.3f per kWh)\n" kwh_needed max_price;
  
  (* Create energy need *)
  let energy_need = 
    Resource.create_field
      ~resource_type:"energy:electricity:solar"
      ~min_quantity:kwh_needed
      ~max_quantity:kwh_needed
      ~quality:Types.Fungible
      ~metadata:[
        "purpose", "home_consumption";
      ]
    |> Result.get_ok
  in
  
  (* Create payment offer *)
  let payment = 
    Resource.create_field
      ~resource_type:"currency:fiat:USD"
      ~min_quantity:kwh_needed  (* Minimum $1 per kWh *)
      ~max_quantity:(kwh_needed *. max_price)
      ~quality:Types.Fungible
      ~metadata:[]
    |> Result.get_ok
  in
  
  match post_intent node ~offers:payment ~wants:energy_need () with
  | Error e -> Printf.printf "Failed to post energy request: %s\n" e
  | Ok intent_id -> Printf.printf "Posted energy request: %s\n" intent_id

(** Grid operator - monitors and facilitates trading *)
let run_grid_operator node =
  Printf.printf "Grid operator monitoring energy trades\n";
  
  let rec monitor () =
    let intents = query_intents node 
      ~resource_type:(Some "energy:electricity:solar") () in
    
    let supply = 
      intents
      |> List.filter (fun i -> 
          String.starts_with ~prefix:"energy" i.Types.offer_field.resource_type)
      |> List.fold_left (fun acc i -> 
          let (_, max_q) = i.Types.offer_field.quantity_range in
          acc +. max_q) 0.0
    in
    
    let demand = 
      intents
      |> List.filter (fun i -> 
          String.starts_with ~prefix:"energy" i.Types.want_field.resource_type)
      |> List.fold_left (fun acc i -> 
          let (_, max_q) = i.Types.want_field.quantity_range in
          acc +. max_q) 0.0
    in
    
    Printf.printf "\n=== Energy Market ===\n";
    Printf.printf "Supply: %.1f kWh available\n" supply;
    Printf.printf "Demand: %.1f kWh requested\n" demand;
    Printf.printf "Balance: %s%.1f kWh\n" 
      (if supply > demand then "+" else "") (supply -. demand);
    
    Unix.sleep 5;
    monitor ()
  in
  monitor ()

(** Smart meter simulation *)
let simulate_smart_meter node household_id =
  Printf.printf "Smart meter for household %s\n" household_id;
  
  let rec loop hour =
    (* Simulate energy production/consumption based on time of day *)
    let solar_production = 
      if hour >= 6 && hour <= 18 then
        (* Daytime - producing solar *)
        let peak = 5.0 in  (* Peak 5kWh at noon *)
        let factor = 1.0 -. (abs_float (12.0 -. float_of_int hour) /. 6.0) in
        peak *. factor
      else 0.0
    in
    
    let consumption = 
      (* Base load + peaks at morning and evening *)
      let base = 0.5 in
      let morning_peak = if hour >= 6 && hour <= 9 then 1.5 else 0.0 in
      let evening_peak = if hour >= 18 && hour <= 22 then 2.0 else 0.0 in
      base +. morning_peak +. evening_peak
    in
    
    let net = solar_production -. consumption in
    
    Printf.printf "[Hour %d] Production: %.1f kWh, Consumption: %.1f kWh, Net: %s%.1f kWh\n"
      hour solar_production consumption (if net >= 0.0 then "+" else "") net;
    
    (* Post appropriate intent based on net position *)
    if net > 0.1 then
      (* Excess energy - sell it *)
      run_energy_producer node net 0.10  (* $0.10 per kWh *)
    else if net < -0.1 then
      (* Need energy - buy it *)
      run_energy_consumer node (abs_float net) 0.15  (* Max $0.15 per kWh *)
    else ();
    
    Unix.sleep 10;  (* Simulate faster for demo *)
    loop ((hour + 1) mod 24)
  in
  loop 6  (* Start at 6 AM *)

let () =
  (* Create node *)
  let node_id = "energy_node_" ^ string_of_int (Random.int 1000) in
  let config = default_config node_id in
  let node = create_node config in
  
  (* Start node *)
  let _ = start node in
  
  (* Parse command line *)
  match Array.to_list Sys.argv with
  | _ :: "producer" :: kwh :: price :: _ ->
      run_energy_producer node (float_of_string kwh) (float_of_string price)
  
  | _ :: "consumer" :: kwh :: max_price :: _ ->
      run_energy_consumer node (float_of_string kwh) (float_of_string max_price)
  
  | _ :: "grid" :: _ ->
      run_grid_operator node
  
  | _ :: "smart-meter" :: household :: _ ->
      simulate_smart_meter node household
  
  | _ ->
      Printf.printf "Energy Trading Network\n";
      Printf.printf "Usage:\n";
      Printf.printf "  %s producer <kWh> <price_per_kWh>\n" Sys.argv.(0);
      Printf.printf "  %s consumer <kWh> <max_price_per_kWh>\n" Sys.argv.(0);
      Printf.printf "  %s grid  (monitor the grid)\n" Sys.argv.(0);
      Printf.printf "  %s smart-meter <household_id>  (simulate smart meter)\n" Sys.argv.(0)