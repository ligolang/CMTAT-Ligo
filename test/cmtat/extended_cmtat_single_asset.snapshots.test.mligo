#import "extended_cmtat_single_asset.instance.mligo" "CMTAT_single_asset"
#import "../helpers/list.mligo" "List_helper"
#import "../helpers/totalsupply_view_caller_contract.mligo" "Caller"
#import "../helpers/isoperator_view_caller_contract.mligo" "Caller_ISOPERATOR"
#import "../helpers/tokenmetadata_view_caller_contract.mligo" "Caller_TOKENMETADATA"
#import "../helpers/getnextsnapshots_view_caller_contract.mligo" "Caller_GETNEXTSNAPSHOTS"
#import "../helpers/snapshottotalsupply_view_caller_contract.mligo" "Caller_SNAPSHOTTOTALSUPPLY"
#import "../helpers/snapshotbalanceof_view_caller_contract.mligo" "Caller_SNAPSHOTBALANCEOF"
#import "../helpers/rule_engine_contract.mligo" "RULE_ENGINE"


let get_initial_storage_ (a, b, c : nat * nat * nat) =
  // let () = Test.reset_state 6n ([] : tez list) in

  let owner1 = Test.nth_bootstrap_account 0 in
  let owner2 = Test.nth_bootstrap_account 1 in
  let owner3 = Test.nth_bootstrap_account 2 in

  let owners = [owner1; owner2; owner3] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

  let ledger = Big_map.literal ([
      (owner1, a);
      (owner2, b);
      (owner3, c);
    ])
  in

  let operators  = Big_map.literal ([
      (owner1, Set.literal [op1]);
      (owner2, Set.literal [op1;op2]);
      (owner3, Set.literal [op1;op3]);
      (op3   , Set.literal [op1;op2]);
    ])
  in

  let token_info = (Map.empty: (string, bytes) map) in
  let token_data = {
    token_id   = 0n;
    token_info = token_info;
  } in
  let token_metadata = Big_map.literal ([
    (0n, token_data);
  ])
  in

 let metadata =Big_map.literal [
	("", [%bytes {|tezos-storage:data|}]);
	("data", [%bytes
{|{
	"name":"FA2",
	"description":"Example FA2 implementation",
	"version":"0.1.0",
	"license":{"name":"MIT"},
	"authors":["Benjamin Fuentes<benjamin.fuentes@marigold.dev>"],
	"homepage":"",
	"source":{"tools":["Ligo"], "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
	"interfaces":["TZIP-012"],
	"errors":[],
	"views":[]

}|}]);
]  in

  let initial_storage: CMTAT_single_asset.storage = {
      ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      administration = { admin = op1; paused = false; killed = false };
      totalsupplies  = a + b + c; //Big_map.literal([(0n, a + b + c)]);
      authorizations = Big_map.empty;
      snapshots = {
        account_snapshots = Big_map.empty;
        totalsupply_snapshots = Map.empty;
        scheduled_snapshots = ([] : timestamp list)
      };
      validation = {
        rule_engine_contract = (None: address option)
      };
      extension = {
        issuer = op2;
      }
  } in
  initial_storage, owners, ops


let get_initial_storage (a, b, c : nat * nat * nat) = 
  let () = Test.reset_state 6n ([] : tez list) in
  get_initial_storage_ (a,b,c)

let get_initial_storage_at (time: timestamp) (a, b, c : nat * nat * nat) = 
  let () = Test.reset_state_at time 6n ([] : tez list) in
  get_initial_storage_ (a,b,c)



let assert_balances
  (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
  let (owner1, balance1) = a in
  let (owner2, balance2) = b in
  let (owner3, balance3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt owner1 ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt owner2 ledger) with
    Some amt ->  assert (amt = balance2)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt owner3 ledger) with
    Some amt -> assert (amt = balance3)
  | None -> failwith "incorret address"
  in
  ()

let assert_no_balances
  (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
  (a, b, c : address * address * address) =
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt a ledger) with
    Some _amt -> failwith "Should not have balance"
  | None -> ()
  in
  let () = match (Big_map.find_opt b ledger) with
    Some _amt -> failwith "Should not have balance"
  | None -> ()
  in
  let () = match (Big_map.find_opt c ledger) with
    Some _amt -> failwith "Should not have balance"
  | None -> ()
  in
  ()

let assert_paused
  (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
  (expected : bool) =
  let storage = Test.get_storage contract_address in
  let administration = storage.administration in
  assert (administration.paused = expected)

let assert_totalsupply
  (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
  (expected : nat) =
  let storage = Test.get_storage contract_address in
  let actual = storage.totalsupplies in
  assert (actual = expected)

let assert_role
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (user : address)
  (expected_role: CMTAT_single_asset.Token.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    match Big_map.find_opt user storage.authorizations with
    | Some(flags) -> assert (Set.mem expected_role flags)
    | None -> failwith "[assert_role] Unknown user"

let assert_not_role
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (user : address)
  (expected_role: CMTAT_single_asset.Token.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    match Big_map.find_opt user storage.authorizations with
    | Some(flags) -> assert (not (Set.mem expected_role flags))
    | None -> failwith "[assert_not_role] Unknown user"

let assert_no_role
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (user : address) =
    let storage = Test.get_storage contract_address in
    match Big_map.find_opt user storage.authorizations with
    | Some(_flags) -> failwith "[assert_no_role] User should not have role"
    | None -> ()


let assert_account_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (time: timestamp)
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
    let storage = Test.get_storage contract_address in
    let () = match Big_map.find_opt a.0 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt time snaps with
        | Some (v) -> assert(a.1 = v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 1 has no snapshot"
    in
    let () = match Big_map.find_opt b.0 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt time snaps with
        | Some (v) -> assert(b.1 = v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 2 has no snapshot"
    in
    let () = match Big_map.find_opt c.0 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt time snaps with
        | Some (v) -> assert(b.1 = v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 3 has no snapshot"
    in
    ()

 
let assert_no_account_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (a, b, c : address * address * address) =
    let storage = Test.get_storage contract_address in
    let () = match Big_map.find_opt a storage.snapshots.account_snapshots with
    | Some(_snaps) -> failwith "Account should not be registered"
    | None -> ()
    in
    let () = match Big_map.find_opt b storage.snapshots.account_snapshots with
    | Some(_snaps) -> failwith "Account should not be registered"
    | None -> ()
    in
      let () = match Big_map.find_opt c storage.snapshots.account_snapshots with
    | Some(_snaps) -> failwith "Account should not be registered"
    | None -> ()
    in
    ()

let assert_totalsupply_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (time: timestamp)
  (expected: nat) =
    let storage = Test.get_storage contract_address in
    let () = match Map.find_opt time storage.snapshots.totalsupply_snapshots with
    | Some (v) -> assert(expected = v)
    | None -> failwith "No total supply snapshot for this time"
    in
    ()


let assert_scheduled_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (time: timestamp) = 
    let storage = Test.get_storage contract_address in
    let () = assert (List_helper.contains time storage.snapshots.scheduled_snapshots)
    in
    ()

let assert_no_scheduled_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (time: timestamp) = 
    let storage = Test.get_storage contract_address in
    assert (not List_helper.contains time storage.snapshots.scheduled_snapshots)


let assert_scheduled_snapshots_contains
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (times: timestamp list) = 
    let storage = Test.get_storage contract_address in
    let contains_one (time: timestamp) = assert (List_helper.contains time storage.snapshots.scheduled_snapshots) in
    List.iter contains_one times 

// Verify that scheduled_snapshot is ordered 
let check_invariant_scheduled_snapshot
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  = 
    let storage = Test.get_storage contract_address in
    match storage.snapshots.scheduled_snapshots with
    | [] -> ()
    | [_] -> ()
    | _::tl -> 
      let merged =  List_helper.zip(storage.snapshots.scheduled_snapshots, tl) in
      List.iter (fun(a, b: timestamp * timestamp) -> assert(a > b)) merged


let assert_rule_engine
  (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
  (expected: address option) = 
    let storage = Test.get_storage contract_address in
    assert (storage.validation.rule_engine_contract = expected)

(* Assert contract call results in failwith with given string *)
let string_failure (res : test_exec_result) (expected : string) : unit =
    let expected = Test.eval expected in
    match res with
        | Fail (Rejected (actual,_)) -> assert (actual = expected)
        | Fail (Balance_too_low _err) -> Test.failwith "contract failed: balance too low"
        | Fail (Other s) -> Test.failwith s
        | Success _ -> Test.failwith "Transaction should fail"




// /////////////////////////////////////////////////////////////////////////////////////////////////////
// //                        SNAPSHOT  VIEWS
// /////////////////////////////////////////////////////////////////////////////////////////////////////


let test_snapshot_balanceof_view_success_simple_fixed_time =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTBALANCEOF) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // SCHEDULESNAPSHOT
  // TIME = "2024-01-01t00:03:06Z"
  let snapshot_time_0 = ("2024-01-01t00:09:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  // TIME = "2024-01-01t00:03:36Z"
  // Keep in mind the owner1 balance before mint
  let storage = Test.get_storage orig.addr in
  let owner1_balance_before_mint = match Big_map.find_opt owner1 storage.ledger with
    Some amt -> amt
  | None -> failwith "Wrong setup ? owner1 has no balance"
  in

  // TIME = "2024-01-01t00:03:36Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // TIME = "2024-01-01t00:05:06Z"
  let () = Test.set_source initial_storage.administration.admin in
  // GRANT op3 the role Minter
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in

  // MINT (with minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 12n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 32n in

  // TIME = "2024-01-01t00:06:02Z"
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 10n), (owner1, 10n), (owner1, 10n))in

  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // TIME = "2024-01-01t00:07:36Z"
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp in the past but after snapshot time)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0 + 60, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in
  ()



// This test creates 2 scheduletimes (t0, t1) and execute a Mint before t0 and a burn between t0 and t1. (we consider the account owner1)
//
//        10  |   12    |   12   |   11   |    11                                 ledger
//            |   10    |   10   |   10   |    10                                 snapshot (t0)
//            |         |        |   12   |    12                                 snapshot (t1)
//        10  |   10    |   10   |   10   |    10                                 view(t0)
//        10  |   12    |   12   |   12   |    12                                 view(t1)
// -----------------------------------------------------------------------------> time
//            ^         ^        ^        ^     
//            |         |        |        |
//          Mint      07:00     Burn     08:00
//            2         t0       1         t1   
        
let test_snapshot_balanceof_view_success_multiple_with_fixed_time =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTBALANCEOF) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // TIME = "2024-01-01t00:03:06Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:09:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  // TIME = "2024-01-01t00:03:36Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-01t00:10:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in


  // Keep in mind the owner1 balance before mint
  let storage = Test.get_storage orig.addr in
  let owner1_balance_before_mint = match Big_map.find_opt owner1 storage.ledger with
    Some amt -> amt
  | None -> failwith "Wrong setup ? owner1 has no balance"
  in

  // TIME = "2024-01-01t00:04:06Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // TIME = "2024-01-01t00:06:06Z"
  let () = Test.set_source initial_storage.administration.admin in
  // GRANT op3 the role Minter
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in
  // MINT (with minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 12n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 32n in

  // TIME = "2024-01-01t00:07:34Z"
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 10n), (owner1, 10n), (owner1, 10n)) in


  let () = Test.set_source initial_storage.administration.admin in
  // GRANT op3 the role Burner
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in


  // BURN (with burner)
  let () = Test.set_source op3 in
  let burn_request = ({ recipient=owner1; token_id=0n; amount=1n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 11n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 31n in


  // TIME = "2024-01-01t00:09:06Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in


  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp after timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in


  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp after timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1 + 1000, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 11n) in
  ()


        
let test_snapshot_balanceof_view_success_multiple_with_bake =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTBALANCEOF) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // TIME = "2024-01-01t00:03:06Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t01:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  // TIME = "2024-01-01t00:03:36Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-01t03:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in


  // Keep in mind the owner1 balance before mint
  let storage = Test.get_storage orig.addr in
  let owner1_balance_before_mint = match Big_map.find_opt owner1 storage.ledger with
    Some amt -> amt
  | None -> failwith "Wrong setup ? owner1 has no balance"
  in

  // TIME = "2024-01-01t00:04:06Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // TIME = "2024-01-01t00:06:06Z"
//  let () = Test.bake_until_n_cycle_end 1n in
  // TIME > "2024-01-01t00:34:00Z"

  let () = Test.set_source initial_storage.administration.admin in
  // GRANT op3 the role Minter
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in

  // MINT (with minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 12n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 32n in
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 10n), (owner1, 10n), (owner1, 10n)) in

  let () = Test.bake_until_n_cycle_end 1n in
  // TIME > "2024-01-01t01:50:00Z"

  let () = Test.set_source initial_storage.administration.admin in
  // GRANT op3 the role Burner
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in

  // BURN (with burner)
  let () = Test.set_source op3 in
  let burn_request = ({ recipient=owner1; token_id=0n; amount=1n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 11n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 31n in

  // Call the "snapshotBalanceOf" view of CMTAT contract
  // Check "snapshotBalanceOf" view at timestamp_0
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in
  // Check "snapshotBalanceOf" view at timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in
  // Check "snapshotBalanceOf" view after timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1 + 1000, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 11n) in
  // Check "snapshotBalanceOf" view between timestamp_0 and timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0 + 100, owner1, 0n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in
  ()


