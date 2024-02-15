#import "extended_cmtat_multi_asset.instance.mligo" "CMTAT_multi_asset"
#import "../helpers/list.mligo" "List_helper"
#import "../helpers/totalsupply_view_caller_contract.mligo" "Caller"
#import "../helpers/snapshotbalanceof_view_caller_contract.mligo" "Caller_SNAPSHOTBALANCEOF"

// alias
module TZIP12 = CMTAT_multi_asset.CMTAT.CMTAT_MULTI_ASSET_EXTENDABLE.TZIP12

let get_initial_storage_ (a, b, c : nat * nat * nat) =
  let owner1 = Test.nth_bootstrap_account 0 in
  let owner2 = Test.nth_bootstrap_account 1 in
  let owner3 = Test.nth_bootstrap_account 2 in

  let owners = [owner1; owner2; owner3] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

  let ledger = Big_map.literal ([
    ((owner1, 1n), a);
    ((owner2, 2n), b);
    ((owner3, 3n), c);
    ((owner1, 2n), a);
  ])
  in

  let operators  = Big_map.literal ([
    ((owner1, op1), Set.literal [1n; 2n]);
    ((owner2, op1), Set.literal [2n]);
    ((owner3, op1), Set.literal [3n]);
    ((op1   , op3), Set.literal [2n]);
  ])
  in

  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : TZIP12.tokenMetadataData));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : TZIP12.tokenMetadataData));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : TZIP12.tokenMetadataData));
  ] : TZIP12.tokenMetadata) in
  
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

  let minter_role : CMTAT_multi_asset.Token.AUTHORIZATIONS.role = MINTER in
  let burner_role : CMTAT_multi_asset.Token.AUTHORIZATIONS.role = BURNER in

  let initial_storage: CMTAT_multi_asset.storage = {
      ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      administration = { admin = op1; paused = false; killed = false };
      totalsupplies  = Big_map.literal([(1n, a); (2n, a + b); (3n, c)]);
      authorizations = {
        general = Big_map.empty;
        specific = Big_map.literal([((op2, 1n, burner_role),()); ((op3, 1n, minter_role), ())]);
      };
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
  (contract_address : (CMTAT_multi_asset parameter_of, CMTAT_multi_asset.storage) typed_address )
  (a, b, c : (address * nat * nat) * (address * nat * nat) * (address * nat * nat)) =
  let (owner1, token_id_1, balance1) = a in
  let (owner2, token_id_2, balance2) = b in
  let (owner3, token_id_3, balance3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt (owner1, token_id_1) ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt (owner2, token_id_2) ledger) with
    Some amt ->  assert (amt = balance2)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt (owner3, token_id_3) ledger) with
    Some amt -> assert (amt = balance3)
  | None -> failwith "incorret address"
  in
  ()


// let assert_no_balances
//   (contract_address : (CMTAT_multi_asset parameter_of, CMTAT_multi_asset.storage) typed_address )
//   (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
//   let storage = Test.get_storage contract_address in
//   let ledger = storage.ledger in
//   let () = match (Big_map.find_opt a ledger) with
//     Some _amt -> failwith "Should not have balance"
//   | None -> ()
//   in
//   let () = match (Big_map.find_opt b ledger) with
//     Some _amt -> failwith "Should not have balance"
//   | None -> ()
//   in
//   let () = match (Big_map.find_opt c ledger) with
//     Some _amt -> failwith "Should not have balance"
//   | None -> ()
//   in
//   ()

// let assert_paused
//   (contract_address : (CMTAT_multi_asset parameter_of, CMTAT_multi_asset.storage) typed_address )
//   (expected : bool) =
//   let storage = Test.get_storage contract_address in
//   let administration = storage.administration in
//   assert (administration.paused = expected)

let assert_totalsupply
  (contract_address : (CMTAT_multi_asset parameter_of, CMTAT_multi_asset.storage) typed_address )
  (token_id : nat)
  (expected : nat) =
  let storage = Test.get_storage contract_address in
  match Big_map.find_opt token_id storage.totalsupplies with 
  | Some(actual) -> assert (actual = expected)
  | None -> failwith "[assert_totalsupply] not total supply for this token_id"
  
// let assert_no_totalsupply
//   (contract_address : (CMTAT_multi_asset parameter_of, CMTAT_multi_asset.storage) typed_address )
//   (token_id : nat) =
//   let storage = Test.get_storage contract_address in
//   match Big_map.find_opt token_id storage.totalsupplies with 
//   | Some(_actual) -> failwith "[assert_no_totalsupply] Should not have a total supply for this token_id"
//   | None -> ()


let assert_role
  (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
  (user : address)
  (token_id_opt: nat option)
  (expected_role: CMTAT_multi_asset.Token.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    assert(CMTAT_multi_asset.Token.AUTHORIZATIONS.hasRole (user, token_id_opt, expected_role) storage.authorizations)


let assert_not_role
  (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
  (user : address)
  (token_id_opt: nat option)
  (expected_role: CMTAT_multi_asset.Token.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    assert(not CMTAT_multi_asset.Token.AUTHORIZATIONS.hasRole (user, token_id_opt, expected_role) storage.authorizations)


// let assert_no_role
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (user : address) =
//     let storage = Test.get_storage contract_address in
//     match Big_map.find_opt user storage.authorizations with
//     | Some(_flags) -> failwith "[assert_no_role] User should not have role"
//     | None -> ()


let assert_account_snapshot
  (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
  (time: timestamp)
  (a, b, c : (address * nat * nat) * (address * nat * nat) * (address * nat * nat)) =
    let (owner1, token_id_1, balance1) = a in
    let (owner2, token_id_2, balance2) = b in
    let (owner3, token_id_3, balance3) = c in
    let storage = Test.get_storage contract_address in
    let () = match Big_map.find_opt owner1 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt (time, token_id_1) snaps with
        | Some (v) -> assert(balance1 = v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 1 has no snapshot"
    in
    let () = match Big_map.find_opt owner2 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt (time, token_id_2) snaps with
        | Some (v) -> assert(balance2 = v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 2 has no snapshot"
    in
    let () = match Big_map.find_opt owner3 storage.snapshots.account_snapshots with
    | Some(snaps) -> 
        let () = match Map.find_opt (time, token_id_3) snaps with
        | Some (v) -> assert(balance3= v)
        | None -> failwith "Account does not have a snapshot for this time"
        in
        ()
    | None -> failwith "[assert_account_snapshot] user 3 has no snapshot"
    in
    ()

 
// let assert_no_account_snapshot
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (a, b, c : address * address * address) =
//     let storage = Test.get_storage contract_address in
//     let () = match Big_map.find_opt a storage.snapshots.account_snapshots with
//     | Some(_snaps) -> failwith "Account should not be registered"
//     | None -> ()
//     in
//     let () = match Big_map.find_opt b storage.snapshots.account_snapshots with
//     | Some(_snaps) -> failwith "Account should not be registered"
//     | None -> ()
//     in
//       let () = match Big_map.find_opt c storage.snapshots.account_snapshots with
//     | Some(_snaps) -> failwith "Account should not be registered"
//     | None -> ()
//     in
//     ()

// let assert_totalsupply_snapshot
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (time: timestamp)
//   (token_id: nat)
//   (expected: nat) =
//     let storage = Test.get_storage contract_address in
//     let () = match Map.find_opt (time, token_id) storage.snapshots.totalsupply_snapshots with
//     | Some (v) -> assert(expected = v)
//     | None -> failwith "No total supply snapshot for this time"
//     in
//     ()


let assert_scheduled_snapshot
  (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
  (time: timestamp) = 
    let storage = Test.get_storage contract_address in
    let () = assert (List_helper.contains time storage.snapshots.scheduled_snapshots)
    in
    ()

// let assert_no_scheduled_snapshot
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (time: timestamp) = 
//     let storage = Test.get_storage contract_address in
//     assert (not List_helper.contains time storage.snapshots.scheduled_snapshots)


// let assert_scheduled_snapshots_contains
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (times: timestamp list) = 
//     let storage = Test.get_storage contract_address in
//     let contains_one (time: timestamp) = assert (List_helper.contains time storage.snapshots.scheduled_snapshots) in
//     List.iter contains_one times 

// // Verify that scheduled_snapshot is ordered 
// let check_invariant_scheduled_snapshot
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   = 
//     let storage = Test.get_storage contract_address in
//     match storage.snapshots.scheduled_snapshots with
//     | [] -> ()
//     | [_] -> ()
//     | _::tl -> 
//       let merged =  List_helper.zip(storage.snapshots.scheduled_snapshots, tl) in
//       List.iter (fun(a, b: timestamp * timestamp) -> assert(a > b)) merged


// let assert_rule_engine
//   (contract_address : ((CMTAT_multi_asset parameter_of), CMTAT_multi_asset.storage) typed_address )
//   (expected: address option) = 
//     let storage = Test.get_storage contract_address in
//     assert (storage.validation.rule_engine_contract = expected)

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
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in
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
  let owner1_balance_before_mint = match Big_map.find_opt (owner1, 1n) storage.ledger with
    Some amt -> amt
  | None -> failwith "Wrong setup ? owner1 has no balance"
  in

  // TIME = "2024-01-01t00:03:36Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // MINT (with minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=1n; amount=2n } : CMTAT_multi_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n, 12n), (owner2, 2n, 10n), (owner3, 3n, 10n)) in
  let () = assert_totalsupply orig.addr 1n 12n in

  // TIME = "2024-01-01t00:06:02Z"
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 1n, 10n), (owner1, 1n, 10n), (owner1, 1n, 10n))in

  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // TIME = "2024-01-01t00:07:36Z"
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp in the past but after snapshot time)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0 + 60, owner1, 1n)) 0tez in
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
//          Mint     00:10:00   Burn   03:00:00
//            2         t0       1        t1   
        
let test_snapshot_balanceof_view_success_multiple_with_bake =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTBALANCEOF) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // TIME = "2024-01-01t00:03:06Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:10:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  // TIME = "2024-01-01t00:03:36Z"
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-01t03:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in


  // Keep in mind the owner1 balance before mint
  let storage = Test.get_storage orig.addr in
  let owner1_balance_before_mint = match Big_map.find_opt (owner1, 1n) storage.ledger with
    Some amt -> amt
  | None -> failwith "Wrong setup ? owner1 has no balance"
  in

  // TIME = "2024-01-01t00:04:06Z"
  // Call View of Caller contract
  // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract (with timestamp_0)
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in

  // MINT (with minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=1n; amount=2n } : CMTAT_multi_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n, 12n), (owner2, 2n, 10n), (owner3, 3n, 10n)) in
  let () = assert_totalsupply orig.addr 1n 12n in
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 1n, 10n), (owner1, 1n, 10n), (owner1, 1n, 10n)) in

  let () = Test.bake_until_n_cycle_end 1n in
  // TIME > "2024-01-01t00:30:00Z"

  // BURN (with burner)
  let () = Test.set_source op2 in
  let burn_request = ({ recipient=owner1; token_id=1n; amount=1n } : CMTAT_multi_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n, 11n), (owner2, 2n, 10n), (owner3, 3n, 10n)) in
  let () = assert_totalsupply orig.addr 1n 11n in

  // Call the "snapshotBalanceOf" view of CMTAT contract
  // Check "snapshotBalanceOf" view at timestamp_0
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = owner1_balance_before_mint) in
  // Check "snapshotBalanceOf" view at timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in
  // Check "snapshotBalanceOf" view after timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_1 + 1000, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 11n) in
  // Check "snapshotBalanceOf" view between timestamp_0 and timestamp_1
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0 + 100, owner1, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 12n) in
  ()


/////////////////////////////////////////////////////////////////////////////////////////
let test_reschedule_snapshot_failure_in_past =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let _op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let _fa2_address : address = Tezos.address contr in
  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:10:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-01t03:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let () = Test.bake_until_n_cycle_end 1n in
  // TIME > "2024-01-01t00:30:00Z"

  // RESCHEDULE
  let snapshot_time_0_resched = ("2024-01-01t00:03:00Z" : timestamp) in
  let r = Test.transfer orig.addr (RescheduleSnapshot (snapshot_time_0, snapshot_time_0_resched)) 0tez in
  let () = string_failure r CMTAT_multi_asset.Token.SNAPSHOTS.Errors.schedule_in_past in
  ()


let test_unschedule_snapshot_failure_in_past =
  let initial_storage, owners, operators = get_initial_storage_at ("2024-01-01t00:00:00Z" : timestamp) (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let _op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let _fa2_address : address = Tezos.address contr in
  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:10:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-01t03:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let () = Test.bake_until_n_cycle_end 1n in
  // TIME > "2024-01-01t00:30:00Z"

  // UNSCHEDULE
  let r = Test.transfer orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = string_failure r CMTAT_multi_asset.Token.SNAPSHOTS.Errors.snapshot_already_done in
  ()

