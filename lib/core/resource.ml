(** Resource management module
    
    This module handles resource definitions, transformations, and conversions.
    Resources are the "what" of ambient commerce - anything that can be traded.
    
    Key concepts:
    - Resources exist in an ontology (hierarchy)
    - Resources can be fungible, graded, or unique
    - Resources can transform into other resources
    - Resources have measurable quantities and qualities
*)

open Types

(** Resource ontology - defines relationships between resource types *)
module Ontology = struct
  (** A node in the resource ontology tree *)
  type node = {
    uri: resource_uri;
    name: string;
    description: string;
    parent: resource_uri option;
    attributes: (string * string) list;
    fungible: bool;
    base_unit: string;  (* e.g., "hours", "GB", "USD" *)
  }
  
  (** The ontology is a tree structure *)
  let ontology_tree : (resource_uri, node) Hashtbl.t = Hashtbl.create 100
  
  (** Initialize with base resource types *)
  let init () =
    let add_resource uri name desc parent fungible unit attrs =
      let node = {
        uri = uri;
        name = name;
        description = desc;
        parent = parent;
        attributes = attrs;
        fungible = fungible;
        base_unit = unit;
      } in
      Hashtbl.add ontology_tree uri node
    in
    
    (* Root categories *)
    add_resource "compute" "Compute" "Computational resources" None false "units" [];
    add_resource "storage" "Storage" "Data storage resources" None false "bytes" [];
    add_resource "currency" "Currency" "Monetary instruments" None false "units" [];
    add_resource "network" "Network" "Network/bandwidth resources" None false "bytes" [];
    add_resource "energy" "Energy" "Energy resources" None false "joules" [];
    
    (* Compute subtypes *)
    add_resource "compute:cpu" "CPU" "Central processing compute" 
      (Some "compute") false "core-hours" ["architecture", "x86_64"];
    add_resource "compute:gpu" "GPU" "Graphics processing compute" 
      (Some "compute") false "gpu-hours" [];
    add_resource "compute:gpu:nvidia" "NVIDIA GPU" "NVIDIA graphics processors" 
      (Some "compute:gpu") false "gpu-hours" ["cuda_capable", "true"];
    add_resource "compute:gpu:nvidia:4090" "RTX 4090" "NVIDIA RTX 4090" 
      (Some "compute:gpu:nvidia") false "gpu-hours" 
      ["memory", "24GB"; "cuda_cores", "16384"];
    
    (* Currency subtypes *)
    add_resource "currency:fiat" "Fiat Currency" "Government-issued currency" 
      (Some "currency") false "units" [];
    add_resource "currency:fiat:USD" "US Dollar" "United States Dollar" 
      (Some "currency:fiat") true "USD" [];
    add_resource "currency:crypto" "Cryptocurrency" "Digital currency" 
      (Some "currency") false "units" [];
    add_resource "currency:crypto:USDC" "USD Coin" "USDC Stablecoin"
      (Some "currency:crypto") true "USDC" ["network", "ethereum"];
    add_resource "currency:crypto:ETH" "Ethereum" "Ether cryptocurrency"
      (Some "currency:crypto") true "ETH" ["network", "ethereum"];
    add_resource "currency:crypto:BTC" "Bitcoin" "Bitcoin cryptocurrency"
      (Some "currency:crypto") true "BTC" ["network", "bitcoin"];
    
    (* Test resources for circular trading *)
    for i = 0 to 9 do
      let uri = Printf.sprintf "resource:%d" i in
      let name = Printf.sprintf "Test Resource %d" i in
      let desc = Printf.sprintf "Test resource for circular trading" in
      add_resource uri name desc None false "units" []
    done;

    (* Storage subtypes *)
    add_resource "storage:ssd" "SSD Storage" "Solid state storage" 
      (Some "storage") true "GB" ["iops", "50000"];
    add_resource "storage:hdd" "HDD Storage" "Hard disk storage" 
      (Some "storage") true "GB" ["iops", "200"];
    
    (* Network subtypes *)
    add_resource "network:bandwidth" "Bandwidth" "Network bandwidth" 
      (Some "network") true "Mbps" [];
    add_resource "network:egress" "Egress" "Outbound data transfer"
      (Some "network") true "GB" [];

    (* Test currencies for testing *)
    for i = 0 to 9 do
      let currency_id = Printf.sprintf "currency:test%d" i in
      let name = Printf.sprintf "Test Currency %d" i in
      let description = Printf.sprintf "Test currency for unit tests %d" i in
      add_resource currency_id name description (Some "currency") true "TST" []
    done

  (** Check if a resource type exists *)
  let exists uri = Hashtbl.mem ontology_tree uri
  
  (** Get resource metadata *)
  let get_metadata uri =
    try Some (Hashtbl.find ontology_tree uri)
    with Not_found -> None
  
  (** Check if one resource is a subtype of another *)
  let rec is_subtype_of child_uri parent_uri =
    if child_uri = parent_uri then true
    else
      match get_metadata child_uri with
      | None -> false
      | Some node ->
          match node.parent with
          | None -> false
          | Some parent -> is_subtype_of parent parent_uri
  
  (** Get all parent types of a resource *)
  let rec get_ancestors uri =
    match get_metadata uri with
    | None -> []
    | Some node ->
        match node.parent with
        | None -> []
        | Some parent -> parent :: get_ancestors parent
  
  (** Check if two resources are compatible for exchange *)
  let are_exchangeable uri1 uri2 =
    (* Same type is always exchangeable *)
    uri1 = uri2 ||
    (* Check if one is a subtype of the other *)
    is_subtype_of uri1 uri2 ||
    is_subtype_of uri2 uri1 ||
    (* Check if they share a common tradeable ancestor *)
    let ancestors1 = get_ancestors uri1 in
    let ancestors2 = get_ancestors uri2 in
    List.exists (fun a -> List.mem a ancestors2) ancestors1
end

(** Global registry instance *)
let global_registry = Registry.create ()

(** Initialize with common resources *)
let () = Registry.seed_common_resources global_registry

(** Get the global registry *)
let get_registry () = global_registry

(** Validate a resource URI - now just checks if it's non-empty *)
let validate_uri uri =
  if String.length uri > 0 then
    Ok ()
  else
    Error "Resource URI cannot be empty"

(** Create a resource field - now permissive *)
let create_field ~resource_type ~min_quantity ~max_quantity ~quality ~metadata =
  (* Validate URI *)
  match validate_uri resource_type with
  | Error e -> Error e
  | Ok () ->
    (* Validate quantities *)
    if min_quantity < 0.0 then
      Error "Minimum quantity cannot be negative"
    else if max_quantity < min_quantity then
      Error "Maximum quantity must be >= minimum quantity"
    else
      (* Auto-discover new resource types *)
      let () = if not (Registry.exists global_registry resource_type) then
        Registry.discover global_registry
          ~uri:resource_type
          ~agent_id:"system"
          ~description:"Auto-discovered resource"
          ()
      in

      (* Accept any quality specification *)
      match quality with
      | Graded score when score < 0.0 || score > 1.0 ->
          Error "Quality score must be between 0.0 and 1.0"
      | _ ->
              Ok {
                resource_type = resource_type;
                quantity_range = (min_quantity, max_quantity);
                quality = quality;
                metadata = metadata;
              }

(** Check if two resource fields can potentially match *)
let fields_compatible offer_field want_field =
  (* Check if resource types are exchangeable *)
  Ontology.are_exchangeable offer_field.resource_type want_field.resource_type &&
  
  (* Check if quantities overlap *)
  let (offer_min, offer_max) = offer_field.quantity_range in
  let (want_min, want_max) = want_field.quantity_range in
  offer_max >= want_min && want_max >= offer_min &&
  
  (* Check if qualities are compatible *)
  match offer_field.quality, want_field.quality with
  | Fungible, Fungible -> true
  | Graded offered, Graded wanted -> offered >= wanted
  | Unique id1, Unique id2 -> id1 = id2
  | Fungible, Graded _ -> true  (* Fungible satisfies any quality *)
  | _ -> false

(** Calculate the actual tradeable quantity range *)
let calculate_tradeable_range offer_field want_field =
  let (offer_min, offer_max) = offer_field.quantity_range in
  let (want_min, want_max) = want_field.quantity_range in
  
  (* The intersection of the ranges *)
  let tradeable_min = max offer_min want_min in
  let tradeable_max = min offer_max want_max in
  
  if tradeable_min <= tradeable_max then
    Some (tradeable_min, tradeable_max)
  else
    None

(** Resource transformation rules *)
module Transformations = struct
  (** A transformation from one resource to another *)
  type transformation = {
    from_resource: resource_uri;
    to_resource: resource_uri;
    rate: float;  (* How many 'to' units per 'from' unit *)
    conditions: (string * string) list;  (* Required conditions *)
    reversible: bool;
  }
  
  let transformations : transformation list ref = ref []
  
  (** Register a transformation *)
  let register ~from_resource ~to_resource ~rate ?(conditions=[]) ?(reversible=false) () =
    let transform = {
      from_resource = from_resource;
      to_resource = to_resource;
      rate = rate;
      conditions = conditions;
      reversible = reversible;
    } in
    transformations := transform :: !transformations;
    
    (* If reversible, also add the reverse transformation *)
    if reversible && rate > 0.0 then
      let reverse = {
        from_resource = to_resource;
        to_resource = from_resource;
        rate = 1.0 /. rate;
        conditions = conditions;
        reversible = false;  (* Avoid infinite recursion *)
      } in
      transformations := reverse :: !transformations
  
  (** Find transformation path between two resources *)
  let find_transformation_path from_res to_res =
    (* Simple BFS to find transformation path *)
    let rec bfs visited queue =
      match queue with
      | [] -> None
      | (current, path, rate) :: rest ->
          if current = to_res then
            Some (List.rev path, rate)
          else if List.mem current visited then
            bfs visited rest
          else
            let next_steps = 
              !transformations
              |> List.filter (fun t -> t.from_resource = current)
              |> List.map (fun t -> 
                  (t.to_resource, t :: path, rate *. t.rate))
            in
            bfs (current :: visited) (rest @ next_steps)
    in
    bfs [] [(from_res, [], 1.0)]
  
  (** Initialize some standard transformations *)
  let init () =
    (* Compute transformations - different GPU types have different performance *)
    register ~from_resource:"compute:gpu:nvidia:4090" 
             ~to_resource:"compute:gpu:nvidia"
             ~rate:1.2  (* 4090 is 20% more powerful than generic nvidia *)
             ();
    
    (* Storage transformations - SSD can substitute for HDD *)
    register ~from_resource:"storage:ssd"
             ~to_resource:"storage:hdd"
             ~rate:1.0  (* SSD can replace HDD 1:1 *)
             ();
    
    (* Currency transformations would be dynamic based on exchange rates *)
    register ~from_resource:"currency:crypto:USDC"
             ~to_resource:"currency:fiat:USD"
             ~rate:0.999  (* Small fee for conversion *)
             ~reversible:true
             ()
end

let () = Transformations.init ()

(** Calculate exchange rate between two resources *)
let calculate_exchange_rate from_resource to_resource =
  if from_resource = to_resource then
    Some 1.0
  else
    match Transformations.find_transformation_path from_resource to_resource with
    | Some (_, rate) -> Some rate
    | None -> None

(** Resource quantity representation with units *)
type quantity = {
  amount: float;
  unit: string;
  resource_type: resource_uri;
}

(** Convert between different units of the same resource type *)
let convert_units quantity ~to_unit =
  (* This would use a proper unit conversion library in production *)
  match quantity.unit, to_unit with
  | "GB", "MB" -> { quantity with amount = quantity.amount *. 1024.0; unit = to_unit }
  | "MB", "GB" -> { quantity with amount = quantity.amount /. 1024.0; unit = to_unit }
  | "hours", "minutes" -> { quantity with amount = quantity.amount *. 60.0; unit = to_unit }
  | "minutes", "hours" -> { quantity with amount = quantity.amount /. 60.0; unit = to_unit }
  | from_u, to_u when from_u = to_u -> quantity
  | _ -> quantity  (* No conversion available *)

(** Pretty print a resource field for debugging *)
let field_to_string field =
  let (min_q, max_q) = field.quantity_range in
  let quality_str = match field.quality with
    | Fungible -> "fungible"
    | Graded g -> Printf.sprintf "graded(%.2f)" g
    | Unique id -> Printf.sprintf "unique(%s)" id
  in
  Printf.sprintf "%s [%.2f-%.2f] %s" 
    field.resource_type min_q max_q quality_str

(** Statistics about resource usage *)
type resource_stats = {
  total_volume: float;  (* Total amount transacted *)
  transaction_count: int;
  average_price: float option;
  price_range: (float * float) option;
}

(** Calculate statistics from a list of settlements *)
let calculate_stats settlements _resource_type =
  let relevant =
    settlements
    |> List.filter (fun _s ->
        (* Check if this settlement involved the resource type *)
        true  (* Simplified - would check actual settlement details *)
      )
  in
  
  let total_volume = 
    List.fold_left (fun acc s -> acc +. s.executed_point.quantity) 0.0 relevant
  in
  
  let prices = 
    List.map (fun s -> s.executed_point.price) relevant
  in
  
  let average_price = 
    match prices with
    | [] -> None
    | ps -> Some (List.fold_left (+.) 0.0 ps /. float_of_int (List.length ps))
  in
  
  let price_range = 
    match prices with
    | [] -> None
    | ps -> 
        let min_p = List.fold_left min infinity ps in
        let max_p = List.fold_left max neg_infinity ps in
        Some (min_p, max_p)
  in
  
  {
    total_volume = total_volume;
    transaction_count = List.length relevant;
    average_price = average_price;
    price_range = price_range;
  }