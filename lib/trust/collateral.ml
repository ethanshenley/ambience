(** Collateral Module
    
    This module manages collateral requirements and staking for the protocol.
    Collateral ensures agents have "skin in the game" and can cover losses.
    
    Key concepts:
    - Collateral calculation
    - Staking and unstaking
    - Slashing conditions
    - Collateral pools
    - Dynamic collateral adjustment
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent  (* Import Intent module for generate_uuid *)

(** Collateral types *)
type collateral_type =
  | Fixed of float                    (* Fixed amount *)
  | Percentage of float                (* Percentage of transaction value *)
  | Dynamic of (float -> float -> float)  (* Function of value and reputation *)
  | Tiered of tier list               (* Different tiers *)

and tier = {
  min_value: float;
  max_value: float;
  collateral_rate: float;
}

(** Staking record *)
type stake = {
  stake_id: uuid;
  staker: public_key;
  amount: float;
  resource: resource_field;
  
  (* Status *)
  locked: bool;
  locked_until: timestamp option;
  locked_for: uuid option;  (* Settlement or intent ID *)
  
  (* Staking period *)
  staked_at: timestamp;
  unstake_requested: timestamp option;
  unstake_delay: float;  (* Delay before can withdraw *)
  
  (* Slashing *)
  slashed_amount: float;
  slashing_history: slashing_event list;
}

and slashing_event = {
  slash_timestamp: timestamp;  (* Renamed to avoid potential conflicts *)
  slash_amount: float;  (* Renamed to avoid conflict with stake.slashed_amount *)
  reason: slashing_reason;
  settlement_id: uuid option;
  slashed_by: public_key;
}

and slashing_reason =
  | SettlementDefault
  | FraudDetected
  | ProtocolViolation
  | DisputeLost
  | InactivityPenalty

(** Collateral pool *)
type pool = {
  pool_id: uuid;
  name: string;
  
  (* Pool parameters *)
  min_stake: float;
  max_stake: float option;
  total_staked: float;
  total_slashed: float;
  
  (* Members *)
  members: (public_key, stake) Hashtbl.t;
  
  (* Rewards *)
  reward_rate: float;  (* Annual percentage *)
  total_rewards_paid: float;
  
  (* Risk parameters *)
  collateral_type: collateral_type;
  slashing_conditions: slashing_condition list;
}

and slashing_condition = {
  condition_type: condition_type;
  penalty_rate: float;  (* 0.0 to 1.0 *)
  max_penalty: float option;
}

and condition_type =
  | OnDefault
  | OnDispute
  | OnInactivity of float  (* Days of inactivity *)
  | OnViolation of string

(** Collateral manager *)
type collateral_manager = {
  pools: (uuid, pool) Hashtbl.t;
  stakes_by_agent: (public_key, stake list) Hashtbl.t;
  
  (* Configuration *)
  default_collateral_type: collateral_type;
  min_collateral_ratio: float;  (* Minimum collateral/value ratio *)
  unstake_delay: float;  (* Default unstake delay *)
  
  (* Statistics *)
  mutable total_staked: float;
  mutable total_slashed: float;
  mutable total_rewards_paid: float;
}

(** Create collateral manager *)
let create_manager ?(default_type = Percentage 0.1) 
    ?(min_ratio = 0.05) ?(unstake_delay = 86400.0 *. 7.0) () =
  {
    pools = Hashtbl.create 10;
    stakes_by_agent = Hashtbl.create 100;
    default_collateral_type = default_type;
    min_collateral_ratio = min_ratio;
    unstake_delay = unstake_delay;
    total_staked = 0.0;
    total_slashed = 0.0;
    total_rewards_paid = 0.0;
  }

(** {2 Collateral Calculation} *)

(** Calculate required collateral *)
let calculate_required manager value reputation collateral_type =
  match collateral_type with
  | Fixed amount -> amount
  | Percentage rate -> value *. rate
  | Dynamic f -> f value reputation
  | Tiered tiers ->
      (* Find applicable tier *)
      List.find_opt (fun tier ->
        value >= tier.min_value && value <= tier.max_value
      ) tiers
      |> Option.map (fun tier -> value *. tier.collateral_rate)
      |> Option.value ~default:(value *. manager.min_collateral_ratio)

(** Check if agent has sufficient collateral *)
let has_sufficient_collateral manager agent required_amount =
  match Hashtbl.find_opt manager.stakes_by_agent agent with
  | None -> false
  | Some stakes ->
      let available = 
        List.fold_left (fun acc stake ->
          if not stake.locked then
            acc +. (stake.amount -. stake.slashed_amount)
          else
            acc
        ) 0.0 stakes
      in
      available >= required_amount

(** {2 Pool Management} *)

(** Create collateral pool *)
let create_pool manager ~name ~min_stake ~collateral_type 
    ?(max_stake = None) ?(reward_rate = 0.05) () =
  
  let pool_id = Ambience_core.Intent.generate_uuid () in
  
  let pool = {
    pool_id = pool_id;
    name = name;
    min_stake = min_stake;
    max_stake = max_stake;
    total_staked = 0.0;
    total_slashed = 0.0;
    members = Hashtbl.create 100;
    reward_rate = reward_rate;
    total_rewards_paid = 0.0;
    collateral_type = collateral_type;
    slashing_conditions = [];
  } in
  
  Hashtbl.add manager.pools pool_id pool;
  Ok pool_id

(** Add slashing condition to pool *)
let add_slashing_condition manager pool_id condition =
  match Hashtbl.find_opt manager.pools pool_id with
  | None -> Error "Pool not found"
  | Some pool ->
      let updated = {
        pool with
        slashing_conditions = condition :: pool.slashing_conditions;
      } in
      Hashtbl.replace manager.pools pool_id updated;
      Ok ()

(** {2 Staking Operations} *)

(** Stake collateral *)
let stake manager agent amount resource ?(pool_id = None) () =
  let stake_id = Intent.generate_uuid () in
  let current_time = Ambience_core.Time_provider.now () in
  
  let stake = {
    stake_id = stake_id;
    staker = agent;
    amount = amount;
    resource = resource;
    locked = false;
    locked_until = None;
    locked_for = None;
    staked_at = current_time;
    unstake_requested = None;
    unstake_delay = manager.unstake_delay;
    slashed_amount = 0.0;
    slashing_history = [];
  } in
  
  (* Add to pool if specified *)
  (match pool_id with
   | None -> ()
   | Some pid ->
       match Hashtbl.find_opt manager.pools pid with
       | None -> ()
       | Some pool ->
           if amount < pool.min_stake then
             failwith "Below minimum stake"
           else if pool.max_stake <> None && 
                  amount > Option.get pool.max_stake then
             failwith "Above maximum stake"
           else
             (* Add to pool *)
             Hashtbl.add pool.members agent stake;
             let updated = {
               pool with
               total_staked = pool.total_staked +. amount;
             } in
             Hashtbl.replace manager.pools pid updated);
  
  (* Track by agent *)
  let agent_stakes = 
    try Hashtbl.find manager.stakes_by_agent agent
    with Not_found -> []
  in
  Hashtbl.replace manager.stakes_by_agent agent (stake :: agent_stakes);
  
  (* Update global stats *)
  manager.total_staked <- manager.total_staked +. amount;
  
  Ok stake_id

(** Lock stake for settlement *)
let lock_stake manager stake_id settlement_id duration =
  (* Find stake across all agents *)
  let stake_opt = 
    Hashtbl.fold (fun _agent stakes acc ->
      match acc with
      | Some _ -> acc
      | None ->
          List.find_opt (fun s -> s.stake_id = stake_id) stakes
    ) manager.stakes_by_agent None
  in
  
  match stake_opt with
  | None -> Error "Stake not found"
  | Some stake ->
      if stake.locked then
        Error "Stake already locked"
      else
        let updated = {
          stake with
          locked = true;
          locked_until = Some (Ambience_core.Time_provider.now () +. duration);
          locked_for = Some settlement_id;
        } in
        
        (* Update in agent's stakes *)
        let agent_stakes = 
          Hashtbl.find manager.stakes_by_agent stake.staker
        in
        let updated_stakes = 
          List.map (fun s ->
            if s.stake_id = stake_id then updated else s
          ) agent_stakes
        in
        Hashtbl.replace manager.stakes_by_agent stake.staker updated_stakes;
        
        Ok ()

(** Request unstaking *)
let request_unstake manager agent stake_id =
  match Hashtbl.find_opt manager.stakes_by_agent agent with
  | None -> Error "Agent not found"
  | Some stakes ->
      match List.find_opt (fun s -> s.stake_id = stake_id) stakes with
      | None -> Error "Stake not found"
      | Some stake ->
          if stake.staker <> agent then
            Error "Not stake owner"
          else if stake.locked then
            Error "Stake is locked"
          else if stake.unstake_requested <> None then
            Error "Unstake already requested"
          else
            let updated = {
              stake with
              unstake_requested = Some (Ambience_core.Time_provider.now ());
            } in
            
            let updated_stakes = 
              List.map (fun s ->
                if s.stake_id = stake_id then updated else s
              ) stakes
            in
            Hashtbl.replace manager.stakes_by_agent agent updated_stakes;
            
            Ok ()

(** Complete unstaking *)
let complete_unstake manager agent stake_id =
  match Hashtbl.find_opt manager.stakes_by_agent agent with
  | None -> Error "Agent not found"
  | Some stakes ->
      match List.find_opt (fun s -> s.stake_id = stake_id) stakes with
      | None -> Error "Stake not found"
      | Some stake ->
          match stake.unstake_requested with
          | None -> Error "Unstake not requested"
          | Some requested_at ->
              let current_time = Ambience_core.Time_provider.now () in
              let elapsed = current_time -. requested_at in
              
              if elapsed < stake.unstake_delay then
                Error "Unstake delay not met"
              else
                (* Remove stake *)
                let remaining_stakes = 
                  List.filter (fun s -> s.stake_id <> stake_id) stakes
                in
                Hashtbl.replace manager.stakes_by_agent agent remaining_stakes;
                
                (* Update global stats *)
                let returned = stake.amount -. stake.slashed_amount in
                manager.total_staked <- manager.total_staked -. stake.amount;
                
                Ok returned

(** {2 Slashing} *)

(** Slash stake *)
let slash_stake manager stake_id amount reason slashed_by 
    ?(settlement_id = None) () =
  
  (* Find stake *)
  let stake_opt = 
    Hashtbl.fold (fun agent stakes acc ->
      match acc with
      | Some _ -> acc
      | None ->
          List.find_opt (fun s -> s.stake_id = stake_id) stakes
          |> Option.map (fun s -> (agent, s))
    ) manager.stakes_by_agent None
  in
  
  match stake_opt with
  | None -> Error "Stake not found"
  | Some (agent, stake) ->
      let available = stake.amount -. stake.slashed_amount in
      let slash_amount = min amount available in
      
      if slash_amount <= 0.0 then
        Error "No funds to slash"
      else
        let slashing_event = {
          slash_timestamp = Ambience_core.Time_provider.now ();
          slash_amount = slash_amount;
          reason = reason;
          settlement_id = settlement_id;
          slashed_by = slashed_by;
        } in
        
        let updated = {
          stake with
          slashed_amount = stake.slashed_amount +. slash_amount;
          slashing_history = slashing_event :: stake.slashing_history;
        } in
        
        (* Update stake *)
        let agent_stakes = Hashtbl.find manager.stakes_by_agent agent in
        let updated_stakes = 
          List.map (fun s ->
            if s.stake_id = stake_id then updated else s
          ) agent_stakes
        in
        Hashtbl.replace manager.stakes_by_agent agent updated_stakes;
        
        (* Update global stats *)
        manager.total_slashed <- manager.total_slashed +. slash_amount;
        
        Ok slash_amount

(** Apply slashing conditions *)
let apply_slashing_conditions manager pool_id event =
  match Hashtbl.find_opt manager.pools pool_id with
  | None -> Error "Pool not found"
  | Some pool ->
      (* Check each condition *)
      List.iter (fun condition ->
        let should_slash = 
          match condition.condition_type, event with
          | OnDefault, SettlementDefault -> true
          | OnDispute, DisputeLost -> true
          | _ -> false
        in
        
        if should_slash then
          (* Slash all pool members proportionally *)
          Hashtbl.iter (fun agent stake ->
            let penalty = stake.amount *. condition.penalty_rate in
            let penalty = 
              match condition.max_penalty with
              | None -> penalty
              | Some max_p -> min penalty max_p
            in
            
            let _ = slash_stake manager stake.stake_id penalty 
                     event "pool_slashing" () in
            ()
          ) pool.members
      ) pool.slashing_conditions;
      
      Ok ()

(** {2 Rewards} *)

(** Calculate staking rewards *)
let calculate_rewards manager agent =
  match Hashtbl.find_opt manager.stakes_by_agent agent with
  | None -> 0.0
  | Some stakes ->
      List.fold_left (fun acc stake ->
        let current_time = Ambience_core.Time_provider.now () in
        let staking_duration = current_time -. stake.staked_at in
        let years = staking_duration /. (365.0 *. 86400.0) in
        
        (* Find pool for reward rate *)
        let reward_rate = 
          Hashtbl.fold (fun _pid pool rate ->
            if Hashtbl.mem pool.members agent then
              pool.reward_rate
            else
              rate
          ) manager.pools 0.05  (* Default 5% *)
        in
        
        let effective_stake = stake.amount -. stake.slashed_amount in
        let reward = effective_stake *. reward_rate *. years in
        acc +. reward
      ) 0.0 stakes

(** Pay rewards *)
let pay_rewards manager agent =
  let rewards = calculate_rewards manager agent in
  manager.total_rewards_paid <- manager.total_rewards_paid +. rewards;
  rewards

(** {2 Statistics} *)

type collateral_stats = {
  total_staked: float;
  total_slashed: float;
  total_rewards_paid: float;
  active_stakes: int;
  pools_count: int;
  average_stake_duration: float;
}

let get_stats manager =
  let all_stakes = 
    Hashtbl.fold (fun _agent stakes acc ->
      acc @ stakes
    ) manager.stakes_by_agent []
  in
  
  let current_time = Ambience_core.Time_provider.now () in
  let total_duration = 
    List.fold_left (fun acc stake ->
      acc +. (current_time -. stake.staked_at)
    ) 0.0 all_stakes
  in
  
  let avg_duration = 
    if List.length all_stakes > 0 then
      total_duration /. float_of_int (List.length all_stakes) /. 86400.0  (* Days *)
    else
      0.0
  in
  
  {
    total_staked = manager.total_staked;
    total_slashed = manager.total_slashed;
    total_rewards_paid = manager.total_rewards_paid;
    active_stakes = List.length all_stakes;
    pools_count = Hashtbl.length manager.pools;
    average_stake_duration = avg_duration;
  }