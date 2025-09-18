(** Ambient Protocol CLI Tool *)

open Ambient_core
open Ambient
open Cmdliner

let node = ref None

(** Initialize node *)
let init_node node_id =
  match !node with
  | Some n -> n
  | None ->
      let config = default_config node_id in
      let n = create_node config in
      let _ = start n in
      node := Some n;
      n

(** Post intent command *)
let post_intent_cmd =
  let offers = 
    let doc = "Resource to offer (e.g., compute:gpu:nvidia:4090)" in
    Arg.(required & opt (some string) None & info ["o"; "offers"] ~doc)
  in
  
  let wants = 
    let doc = "Resource wanted (e.g., currency:fiat:USD)" in
    Arg.(required & opt (some string) None & info ["w"; "wants"] ~doc)
  in
  
  let quantity = 
    let doc = "Quantity to trade" in
    Arg.(value & opt float 1.0 & info ["q"; "quantity"] ~doc)
  in
  
  let price = 
    let doc = "Price per unit" in
    Arg.(value & opt float 10.0 & info ["p"; "price"] ~doc)
  in
  
  let run offers wants quantity price =
    let n = init_node "cli_node" in
    
    (* Create resource fields *)
    let offer_field = 
      Resource.create_field
        ~resource_type:offers
        ~min_quantity:quantity
        ~max_quantity:quantity
        ~quality:Types.Fungible
        ~metadata:[]
      |> Result.get_ok
    in
    
    let want_field = 
      Resource.create_field
        ~resource_type:wants
        ~min_quantity:(quantity *. price)
        ~max_quantity:(quantity *. price)
        ~quality:Types.Fungible
        ~metadata:[]
      |> Result.get_ok
    in
    
    match post_intent n ~offers:offer_field ~wants:want_field () with
    | Error e -> Printf.printf "Error: %s\n" e
    | Ok id -> Printf.printf "Posted intent: %s\n" id
  in
  
  let doc = "Post a new intent to the network" in
  let info = Cmd.info "post" ~doc in
  Cmd.v info Term.(const run $ offers $ wants $ quantity $ price)

(** List intents command *)
let list_intents_cmd =
  let resource = 
    let doc = "Filter by resource type" in
    Arg.(value & opt (some string) None & info ["r"; "resource"] ~doc)
  in
  
  let limit = 
    let doc = "Maximum results" in
    Arg.(value & opt int 10 & info ["l"; "limit"] ~doc)
  in
  
  let run resource limit =
    let n = init_node "cli_node" in
    let intents = query_intents n ?resource_type:resource ~max_results:limit () in
    
    Printf.printf "Active Intents:\n";
    Printf.printf "%-36s %-20s %-20s %-10s\n" "ID" "Offers" "Wants" "Priority";
    Printf.printf "%s\n" (String.make 90 '-');
    
    List.iter (fun i ->
      Printf.printf "%-36s %-20s %-20s %8.2f\n"
        (String.sub i.Types.intent_id 0 36)
        i.Types.offer_field.resource_type
        i.Types.want_field.resource_type
        i.Types.priority
    ) intents;
    
    Printf.printf "\nTotal: %d intents\n" (List.length intents)
  in
  
  let doc = "List active intents" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ resource $ limit)

(** Stats command *)
let stats_cmd =
  let run () =
    let n = init_node "cli_node" in
    let stats = get_node_stats n in
    Printf.printf "%s\n" (Yojson.Safe.pretty_to_string stats)
  in
  
  let doc = "Show node statistics" in
  let info = Cmd.info "stats" ~doc in
  Cmd.v info Term.(const run $ const ())

(** Monitor command *)
let monitor_cmd =
  let interval = 
    let doc = "Update interval in seconds" in
    Arg.(value & opt int 5 & info ["i"; "interval"] ~doc)
  in
  
  let run interval =
    let n = init_node "cli_node" in
    
    let rec loop () =
      (* Clear screen *)
      Printf.printf "\027[2J\027[H";
      
      let stats = get_node_stats n in
      Printf.printf "=== Ambient Protocol Monitor ===\n\n";
      Printf.printf "%s\n" (Yojson.Safe.pretty_to_string stats);
      
      Unix.sleep interval;
      loop ()
    in
    loop ()
  in
  
  let doc = "Monitor node activity" in
  let info = Cmd.info "monitor" ~doc in
  Cmd.v info Term.(const run $ interval)

(** Main command *)
let main_cmd =
  let doc = "Ambient Commerce Protocol CLI" in
  let info = Cmd.info "ambient" ~doc in
  let subcommands = [
    post_intent_cmd;
    list_intents_cmd;
    stats_cmd;
    monitor_cmd;
  ] in
  Cmd.group info subcommands

let () = 
  exit (Cmd.eval main_cmd)