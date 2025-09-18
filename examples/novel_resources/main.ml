(** Novel Resources Example

    Demonstrates the dynamic resource registry with creative resource types
    that emerge organically from agent interactions.
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent
module Resource = Ambience_core.Resource
module Registry = Ambience_core.Registry
module State = Ambience_core.State
module Engine = Ambience_matching.Engine

(** Helper to create resource fields *)
let create_resource ~uri ~quantity ~quality =
  match Resource.create_field
    ~resource_type:uri
    ~min_quantity:quantity
    ~max_quantity:quantity
    ~quality
    ~metadata:[]
  with
  | Ok field -> field
  | Error e -> failwith ("Failed to create resource: " ^ e)

(** Scenario 1: Social Media Capital Trading *)
let scenario_social_capital registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       Scenario 1: Social Media Capital Exchange          ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  (* Alice has Twitter influence *)
  let alice_resource = "social:twitter:followers:verified" in
  Registry.discover registry
    ~uri:alice_resource
    ~agent_id:"alice"
    ~description:"Verified Twitter followers"
    ~properties:[("platform", "twitter"); ("verification", "blue_check")]
    ();

  Printf.printf "🆕 Alice discovers new resource: %s\n" alice_resource;

  (* Bob wants social reach for his startup *)
  let bob_wants = "social:reach:targeted:tech" in
  Registry.discover registry
    ~uri:bob_wants
    ~agent_id:"bob"
    ~description:"Targeted social media reach in tech community"
    ();

  Printf.printf "🆕 Bob discovers new resource: %s\n" bob_wants;

  (* Charlie endorses Alice's resource as legitimate *)
  let _ = Registry.endorse registry
    ~uri:alice_resource
    ~agent_id:"charlie"
    ~stake:5.0
    ~notes:"I've successfully traded with Alice for social promotion"
    () in

  Printf.printf "✅ Charlie endorses '%s' with 5.0 reputation stake\n" alice_resource;

  (* Create intents *)
  let alice_intent = Intent.create
    ~agent_id:"alice"
    ~offers:(create_resource ~uri:alice_resource ~quantity:10000.0 ~quality:Fungible)
    ~wants:(create_resource ~uri:"currency:crypto:USDC" ~quantity:500.0 ~quality:Fungible)
    ~constraints:[]
    () in

  let bob_intent = Intent.create
    ~agent_id:"bob"
    ~offers:(create_resource ~uri:"currency:crypto:USDC" ~quantity:500.0 ~quality:Fungible)
    ~wants:(create_resource ~uri:bob_wants ~quantity:10000.0 ~quality:(Graded 0.8))
    ~constraints:[]
    () in

  Printf.printf "\nIntents posted:\n";
  Printf.printf "  Alice: 10K verified followers -> 500 USDC\n";
  Printf.printf "  Bob: 500 USDC -> 10K targeted tech reach\n";

  (* Check credibility *)
  let cred_score = Registry.get_credibility_score registry alice_resource in
  Printf.printf "\n📊 Credibility of '%s': %.2f\n" alice_resource cred_score;

  Printf.printf "💡 Agents can now negotiate whether Twitter followers satisfy 'tech reach'\n"

(** Scenario 2: Computational Resources Beyond Traditional *)
let scenario_quantum_compute registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       Scenario 2: Quantum Computing Resources            ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  (* IBM offers quantum computing *)
  let quantum_uri = "compute:quantum:qubits:IBM-Q" in
  Registry.discover registry
    ~uri:quantum_uri
    ~agent_id:"ibm_quantum"
    ~description:"IBM Quantum computer qubit-hours"
    ~properties:[
      ("provider", "IBM");
      ("architecture", "superconducting");
      ("qubits", "127");
      ("coherence_time", "100us")
    ]
    ();

  Printf.printf "🆕 IBM discovers: %s\n" quantum_uri;

  (* Research lab needs quantum compute *)
  let research_intent = Intent.create
    ~agent_id:"mit_lab"
    ~offers:(create_resource ~uri:"currency:fiat:USD" ~quantity:10000.0 ~quality:Fungible)
    ~wants:(create_resource ~uri:quantum_uri ~quantity:100.0 ~quality:(Graded 0.95))
    ~constraints:[]
    () in

  (* Record usage *)
  Registry.record_usage registry ~uri:quantum_uri ~usage_type:`Intent ~success:true;

  Printf.printf "📈 Resource '%s' recorded in intent\n" quantum_uri;

  (* Multiple endorsements build trust *)
  let endorsers = [
    ("stanford_lab", 10.0, "Successfully ran Shor's algorithm");
    ("google_ai", 15.0, "Reliable quantum resource provider");
    ("defense_contractor", 25.0, "Critical for cryptography research");
  ] in

  List.iter (fun (agent, stake, note) ->
    let _ = Registry.endorse registry ~uri:quantum_uri ~agent_id:agent ~stake ~notes:note () in
    Printf.printf "✅ %s endorses with %.1f stake: \"%s\"\n" agent stake note
  ) endorsers;

  let cred = Registry.get_credibility_score registry quantum_uri in
  Printf.printf "\n📊 Quantum resource credibility: %.2f (highly endorsed!)\n" cred

(** Scenario 3: Carbon Credits and Environmental Assets *)
let scenario_carbon_credits registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       Scenario 3: Carbon Credit Marketplace              ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  (* Multiple standards for carbon credits emerge *)
  let carbon_types = [
    ("carbon:offset:verified:gold-standard", "Gold Standard carbon credits", "gold_standard_org");
    ("carbon:offset:verified:verra", "Verra VCS verified carbon credits", "verra_registry");
    ("carbon:removal:dac:climeworks", "Direct air capture CO2 removal", "climeworks");
    ("carbon:offset:nature:rainforest", "Rainforest preservation credits", "rainforest_alliance");
  ] in

  List.iter (fun (uri, desc, agent) ->
    Registry.discover registry ~uri ~agent_id:agent ~description:desc ();
    Printf.printf "🌱 Discovered: %s\n" uri
  ) carbon_types;

  (* Find similar carbon resources *)
  Printf.printf "\n🔍 Finding similar resources to 'carbon:offset:verified:gold-standard':\n";
  let similar = Registry.find_similar registry "carbon:offset:verified:gold-standard" ~limit:3 in
  List.iter (fun (uri, score) ->
    Printf.printf "  - %s (similarity: %.2f)\n" uri score
  ) similar;

  (* Company wants to offset emissions *)
  let company_intent = Intent.create
    ~agent_id:"tech_company"
    ~offers:(create_resource ~uri:"currency:fiat:EUR" ~quantity:50000.0 ~quality:Fungible)
    ~wants:(create_resource ~uri:"carbon:offset:verified:*" ~quantity:1000.0 ~quality:(Graded 0.8))
    ~constraints:[]
    () in

  Printf.printf "\n🏢 Tech company wants any verified carbon offset (using wildcard)\n";
  Printf.printf "   Multiple providers can compete with different standards\n"

(** Scenario 4: Human Attention Economy *)
let scenario_attention_economy registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       Scenario 4: The Attention Economy                  ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  (* Different types of human attention *)
  let attention_types = [
    ("attention:minutes:focused:expert", "Expert-level focused attention", [("expertise", "required")]);
    ("attention:minutes:casual:crowd", "Crowd-sourced casual attention", [("expertise", "none")]);
    ("attention:review:code:senior", "Senior developer code review time", [("skill", "programming")]);
    ("attention:eyeballs:advertisement", "Ad viewing attention", [("medium", "video")]);
  ] in

  List.iter (fun (uri, desc, props) ->
    Registry.discover registry ~uri ~agent_id:"attention_market" ~description:desc ~properties:props ();
    Printf.printf "👁️ Discovered: %s\n" uri
  ) attention_types;

  (* A startup needs code review *)
  let startup = Intent.create
    ~agent_id:"startup"
    ~offers:(create_resource ~uri:"equity:startup:series-a:tokens" ~quantity:100.0 ~quality:(Unique "startup_2024_tokens"))
    ~wants:(create_resource ~uri:"attention:review:code:senior" ~quantity:10.0 ~quality:(Graded 0.9))
    ~constraints:[]
    () in

  Printf.printf "\n🚀 Startup offers equity tokens for senior code review hours\n";

  (* Track that this novel resource is being used *)
  Registry.record_usage registry ~uri:"equity:startup:series-a:tokens" ~usage_type:`Intent ~success:false;
  Printf.printf "📝 Novel resource 'equity tokens' recorded (unproven)\n"

(** Scenario 5: Gaming and Virtual Assets *)
let scenario_gaming_assets registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║       Scenario 5: Cross-Game Virtual Asset Trading       ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  (* Game assets from different games *)
  let game_assets = [
    ("game:wow:gold:server-us-west", "World of Warcraft gold");
    ("game:fortnite:vbucks", "Fortnite V-Bucks");
    ("game:csgo:skin:awp-dragon-lore", "CS:GO AWP Dragon Lore skin");
    ("game:minecraft:account:og-username", "OG Minecraft username");
    ("game:pokemon:card:charizard-1st-edition", "Physical Pokemon card");
  ] in

  List.iter (fun (uri, desc) ->
    Registry.discover registry ~uri ~agent_id:"gamer" ~description:desc ();
    Printf.printf "🎮 Discovered: %s\n" uri
  ) game_assets;

  (* Create cross-game trade *)
  Printf.printf "\n🔄 Cross-game trade opportunity:\n";
  Printf.printf "   WoW player offers: 10000 gold\n";
  Printf.printf "   CS:GO player offers: AWP Dragon Lore skin\n";
  Printf.printf "   Both assets have value in their communities!\n"

(** Scenario 6: Trending Resources *)
let show_trending registry =
  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║            📈 Trending Resources (Last Hour)              ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  let trending = Registry.trending registry ~window:3600.0 ~limit:5 in
  List.iteri (fun i (uri, score) ->
    Printf.printf "%d. %s (score: %.1f)\n" (i+1) uri score
  ) trending;

  Printf.printf "\n📊 Innovation rate: %.1f new resources/day\n"
    (Registry.get_innovation_rate registry)

(** Main demonstration *)
let () =
  Printf.printf "╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║   Dynamic Resource Registry - Novel Resource Types       ║\n";
  Printf.printf "║                                                          ║\n";
  Printf.printf "║   \"Any economic value can be tokenized and traded\"      ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n";

  (* Create registry *)
  let registry = Registry.create () in
  Registry.seed_common_resources registry;

  Printf.printf "\n📚 Registry initialized with %d common resources\n"
    (List.length (Registry.list_all registry));

  (* Run scenarios *)
  scenario_social_capital registry;
  scenario_quantum_compute registry;
  scenario_carbon_credits registry;
  scenario_attention_economy registry;
  scenario_gaming_assets registry;

  (* Show analytics *)
  show_trending registry;

  Printf.printf "\n╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║                    Key Insights                          ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n\n";

  Printf.printf "1️⃣ Resources emerge organically from agent needs\n";
  Printf.printf "2️⃣ Endorsements create trust without central authority\n";
  Printf.printf "3️⃣ Similar resources can compete (carbon credit standards)\n";
  Printf.printf "4️⃣ Novel pairings enable new economic relationships\n";
  Printf.printf "5️⃣ The protocol remains unopinionated about value\n\n";

  Printf.printf "Total unique resources discovered: %d\n"
    (List.length (Registry.list_all registry));

  Printf.printf "\n✨ The economy is limited only by imagination! ✨\n\n"