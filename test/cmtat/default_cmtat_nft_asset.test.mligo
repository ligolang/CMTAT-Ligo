#import "../../lib/main.mligo" "CMTAT"
// use the "final" nft asset (no extension)
module CMTAT_nft_asset = CMTAT.CMTAT_NFT_ASSET

#import "../helpers/list.mligo" "List_helper"
// #import "../helpers/nft_helpers.mligo" "TestHelpers"
#import "../helpers/totalsupply_view_caller_contract.mligo" "Caller"
#import "../helpers/isoperator_view_caller_contract.mligo" "Caller_ISOPERATOR"
#import "../helpers/tokenmetadata_view_caller_contract.mligo" "Caller_TOKENMETADATA"
#import "../helpers/getnextsnapshots_view_caller_contract.mligo" "Caller_GETNEXTSNAPSHOTS"
#import "../helpers/snapshottotalsupply_view_caller_contract.mligo" "Caller_SNAPSHOTTOTALSUPPLY"
#import "../helpers/snapshotbalanceof_view_caller_contract.mligo" "Caller_SNAPSHOTBALANCEOF"
#import "../helpers/rule_engine_bl_contract.mligo" "RULE_ENGINE_BLACKLIST"
#import "../helpers/rule_engine_wl_contract.mligo" "RULE_ENGINE_WHITELIST"


// alias
module TZIP12 = CMTAT.CMTAT_NFT_ASSET_EXTENDABLE.TZIP12

let get_initial_storage () =
  let () = Test.reset_state 8n ([
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
  ] : tez list) in

  let baker = Test.nth_bootstrap_account 7 in
  let () = Test.set_baker baker in

  let owner1 = Test.nth_bootstrap_account 0 in
  let owner2 = Test.nth_bootstrap_account 1 in
  let owner3 = Test.nth_bootstrap_account 2 in
  let owner4 = Test.nth_bootstrap_account 6 in

  let owners = [owner1; owner2; owner3; owner4] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

    let ledger = Big_map.literal ([
    (1n, owner1);
    (2n, owner2);
    (3n, owner3);
    (4n, owner4);
    (5n, owner4);
  ])
  in

  let operators  = Big_map.literal ([
    ((owner1, op1), Set.literal [1n]);
    ((owner2, op1), Set.literal [2n]);
    ((owner3, op1), Set.literal [3n]);
    ((op1   , op3), Set.literal [1n]);
    ((owner4, op1), Set.literal [4n; 5n]);
  ])
  in

  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadataData));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadataData));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadataData));
    (4n, ({token_id=4n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadataData));
    (5n, ({token_id=5n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadataData));
  ] : CMTAT_nft_asset.CmtatNftAssetExtendable.TZIP12.tokenMetadata) in

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
] in

  let minter_role : CMTAT_nft_asset.CmtatNftAssetExtendable.AUTHORIZATIONS.role = MINTER in
  let burner_role : CMTAT_nft_asset.CmtatNftAssetExtendable.AUTHORIZATIONS.role = BURNER in

  let initial_storage : CMTAT_nft_asset.storage = {
      ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      administration = { admin = op1; paused = false; killed = false };
      totalsupplies  = Big_map.literal([(1n, 1n); (2n, 1n); (3n, 1n); (4n, 1n); (5n, 1n)]);
      authorizations = Big_map.literal([((op2, burner_role), ()); ((op3, minter_role), ())]);
      snapshots = {
        account_snapshots = Big_map.empty;
        totalsupply_snapshots = Big_map.empty;
        scheduled_snapshots = ([] : timestamp list)
      };
      validation = {
        rule_engine_contract = (None: address option)
      };
      // extension = {
      //   issuer = op2;
      // }
  } in

  initial_storage, owners, ops


let assert_balances
  (contract_address : (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) typed_address )
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
  let (owner1, token_id_1) = a in
  let (owner2, token_id_2) = b in
  let (owner3, token_id_3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt token_id_1 ledger) with
    Some amt -> assert (amt = owner1)
  | None -> Test.failwith "incorret address"
  in
  let () = match (Big_map.find_opt token_id_2 ledger) with
    Some amt ->  assert (amt = owner2)
  | None -> Test.failwith "incorret address"
  in
  let () = match (Big_map.find_opt token_id_3 ledger) with
    Some amt -> assert (amt = owner3)
  | None -> Test.failwith "incorret address"
  in
  ()

let assert_error (result : test_exec_result) (error : CMTAT_nft_asset.CmtatNftAssetExtendable.FA2.NFTExtendable.Errors.t) =
  match result with
    Success _ -> Test.failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval error))
  | Fail _ -> Test.failwith "invalid test failure"


let assert_not_owned
  (contract_address : (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) typed_address )
  (tok_id: nat ) =
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt tok_id ledger) with
    Some _amt -> failwith "Should not be owned"
  | None -> ()
  in
  ()

let assert_paused
  (contract_address : (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) typed_address )
  (expected : bool) =
  let storage = Test.get_storage contract_address in
  let administration = storage.administration in
  assert (administration.paused = expected)

let assert_totalsupply
  (contract_address : (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) typed_address )
  (token_id : nat)
  (expected : nat) =
  let storage = Test.get_storage contract_address in
  match Big_map.find_opt token_id storage.totalsupplies with 
  | Some(actual) -> assert (actual = expected)
  | None -> assert (0n = expected)
  
let assert_no_totalsupply
  (contract_address : (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) typed_address )
  (token_id : nat) =
  let storage = Test.get_storage contract_address in
  match Big_map.find_opt token_id storage.totalsupplies with 
  | Some(_actual) -> failwith "[assert_no_totalsupply] Should not have a total supply for this token_id"
  | None -> ()

let assert_role
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (user : address)
  (expected_role: CMTAT_nft_asset.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    match Big_map.find_opt (user, expected_role) storage.authorizations with
    | Some(_) -> assert (true)
    | None -> assert(false)

let assert_not_role
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (user : address)
  (expected_role: CMTAT_nft_asset.AUTHORIZATIONS.role) =
    let storage = Test.get_storage contract_address in
    match Big_map.find_opt (user, expected_role) storage.authorizations with
    | Some(_flags) -> assert(false)
    | None -> assert(true)


let assert_account_snapshot
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (time: timestamp)
  (a, b, c : (address * nat * nat) * (address * nat * nat) * (address * nat * nat)) =
    let (owner1, token_id_1, balance1) = a in
    let (owner2, token_id_2, balance2) = b in
    let (owner3, token_id_3, balance3) = c in
    let storage = Test.get_storage contract_address in
    let () = match Big_map.find_opt (owner1, time, token_id_1) storage.snapshots.account_snapshots with
    | Some (v) -> assert(balance1 = v)
    | None -> failwith "[assert_account_snapshot] user 1 has no snapshot"
    in
    let () = match Big_map.find_opt (owner2, time, token_id_2) storage.snapshots.account_snapshots with
    | Some (v) -> assert(balance2 = v)
    | None -> failwith "[assert_account_snapshot] user 2 has no snapshot"
    in
    let () = match Big_map.find_opt (owner3, time, token_id_3) storage.snapshots.account_snapshots with
    | Some (v) -> assert(balance3= v)
    | None -> failwith "[assert_account_snapshot] user 3 has no snapshot"
    in
    ()

let assert_totalsupply_snapshot
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (time: timestamp)
  (token_id: nat)
  (expected: nat) =
    let storage = Test.get_storage contract_address in
    let () = match Big_map.find_opt (time, token_id) storage.snapshots.totalsupply_snapshots with
    | Some (v) -> assert(expected = v)
    | None -> failwith "No total supply snapshot for this time"
    in
    ()


let assert_scheduled_snapshot
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (time: timestamp) = 
    let storage = Test.get_storage contract_address in
    let () = assert (List_helper.contains time storage.snapshots.scheduled_snapshots)
    in
    ()

let assert_no_scheduled_snapshot
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (time: timestamp) = 
    let storage = Test.get_storage contract_address in
    assert (not List_helper.contains time storage.snapshots.scheduled_snapshots)


let assert_scheduled_snapshots_contains
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  (times: timestamp list) = 
    let storage = Test.get_storage contract_address in
    let contains_one (time: timestamp) = assert (List_helper.contains time storage.snapshots.scheduled_snapshots) in
    List.iter contains_one times 

// Verify that scheduled_snapshot is ordered 
let check_invariant_scheduled_snapshot
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
  = 
    let storage = Test.get_storage contract_address in
    match storage.snapshots.scheduled_snapshots with
    | [] -> ()
    | [_] -> ()
    | _::tl -> 
      let merged =  List_helper.zip(storage.snapshots.scheduled_snapshots, tl) in
      List.iter (fun(a, b: timestamp * timestamp) -> assert(a > b)) merged


let assert_rule_engine
  (contract_address : ((CMTAT_nft_asset parameter_of), CMTAT_nft_asset.storage) typed_address )
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


/////////////////////////////////////////////////////////////////////////////////////////////////////
//            ORIGINATION
/////////////////////////////////////////////////////////////////////////////////////////////////////

let test_origination_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let () = assert_balances orig.addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_totalsupply orig.addr 1n 1n in
  let () = assert_totalsupply orig.addr 2n 1n in
  let () = assert_totalsupply orig.addr 3n 1n in
  ()

//////////////////////////////////////////////////////////////////////////////////////////////
//                KILL
//////////////////////////////////////////////////////////////////////////////////////////////

let test_kill_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // KILL
  let _ = Test.transfer_exn orig.addr (Kill ()) 0tez in
  let () = assert_paused orig.addr true in
  let () = assert_not_owned orig.addr 1n in
  let () = assert_not_owned orig.addr 2n in
  let () = assert_not_owned orig.addr 3n in
  let () = assert_no_totalsupply orig.addr 1n in
  let () = assert_no_totalsupply orig.addr 2n in
  let () = assert_no_totalsupply orig.addr 3n in
  let () = assert_rule_engine orig.addr (None: address option) in

  let storage = Test.get_storage orig.addr in
  let () = assert (storage.snapshots.scheduled_snapshots = ([]: timestamp list)) in
  // let () = assert_no_account_snapshot orig.addr (owner1, owner2, owner3) in

  // PAUSE - fails
  let r = Test.transfer orig.addr (Pause false) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.contract_killed in
  let () = assert_paused orig.addr true in

  // GRANT ROLE - fails
  let flag_snapshooter : CMTAT_nft_asset.AUTHORIZATIONS.role = SNAPSHOOTER in
  let r = Test.transfer orig.addr (GrantRole (owner1, flag_snapshooter)) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.contract_killed in

  // TRANSFER - fails
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.contract_killed in
  ()


let test_kill_failure_not_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let () = Test.set_source owner1 in
  // KILL - fails
  let r = Test.transfer orig.addr (Kill ()) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.not_admin in

  let () = Test.set_source initial_storage.administration.admin in
  // GRANT ROLE
  let flag_snapshooter : CMTAT_nft_asset.AUTHORIZATIONS.role = SNAPSHOOTER in
  let _r = Test.transfer_exn orig.addr (GrantRole (owner1, flag_snapshooter)) 0tez in
  // GRANT ROLE
  let flag_ruler : CMTAT_nft_asset.AUTHORIZATIONS.role = RULER in
  let _r = Test.transfer_exn orig.addr (GrantRole (owner1, flag_ruler)) 0tez in
  // GRANT ROLE
  let flag_validator : CMTAT_nft_asset.AUTHORIZATIONS.role = VALIDATOR in
  let _r = Test.transfer_exn orig.addr (GrantRole (owner1, flag_validator)) 0tez in

  // KILL - fails
  let () = Test.set_source owner1 in
  let r = Test.transfer orig.addr (Kill ()) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.not_admin in
  ()

//////////////////////////////////////////////////////////////////////////////////////////////
//                PAUSE (overide)
//////////////////////////////////////////////////////////////////////////////////////////////

let test_pause_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // PAUSE
  let _ = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  ()

let test_pause_failure =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let () = Test.set_source op3 in
  let r = Test.transfer orig.addr (Pause true) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_pauser in
  let () = assert_paused orig.addr false in
  ()

// verify that override works. Pauser role is not enough in this extended contract , only issuer or admin)
let test_pause_success_with_pauser =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // GRANT op3 the role Pauser
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_nft_asset.AUTHORIZATIONS.role = PAUSER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in
  // PAUSE
  let () = Test.set_source op3 in
  let _r = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  ()


/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        TRANSFER
/////////////////////////////////////////////////////////////////////////////////////////////////////

let test_atomic_transfer_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 5n), (owner2, 1n), (owner3, 4n)) in
  ()

let test_atomic_transfer_failure_in_pause =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let _ = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.ADMINISTRATION.Errors.contract_in_pause in
  ()

/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        TOTALSUPPLY VIEW
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_totalsupply_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in

  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in
  // MINT
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 6n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_totalsupply orig.addr 6n 1n in

  // Call View of Caller contract
 // Caller contract calls the "totalSupply" view of CMTAT contract
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address,6n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = 1n) in
  ()

/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        IS_OPERATOR VIEW
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_isoperator_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_ISOPERATOR) (None: bool option) 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // Call View of Caller contract
 // Caller contract calls the "is_opertor" view of CMTAT contract
  let op_request : TZIP12.operator = { owner=owner1; operator=op1; token_id=0n } in
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, op_request)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = Some(true)) in
  ()



/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        TOKEN_METADATA VIEW
/////////////////////////////////////////////////////////////////////////////////////////////////////
type tokenMetadataData = TZIP12.tokenMetadataData

let test_tokenmetadata_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_TOKENMETADATA) (None: tokenMetadataData option) 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // Call View of Caller contract
 // Caller contract calls the "is_opertor" view of CMTAT contract
  let _r = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, 1n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  match storage_caller with
  | Some(data) -> 
    let () = assert (data.token_id=1n) in
    let () = assert (data.token_info=(Map.empty : (string, bytes) map)) in
    ()
  | None -> failwith "[Test] missing token_metadata"


/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        MINT
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_mint_failure_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
	let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // MINT (with admin)
  let () = Test.set_source initial_storage.administration.admin in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let r = Test.transfer orig.addr (Mint mint_request) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_minter in
  ()

let test_mint_success_with_minter =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  // MINT 
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  // let () = assert_balances orig.addr ((owner1, 12n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_balances orig.addr ((owner1, 6n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_totalsupply orig.addr 6n 1n in
  ()

let test_mint_failure_not_minter =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // MINT (fails with not minter)
  let () = Test.set_source op2 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let r = Test.transfer orig.addr (Mint mint_request) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_minter in
  let () = assert_balances orig.addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_not_owned orig.addr 6n in
  let () = assert_totalsupply orig.addr 6n 0n in
  ()

/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        BURN
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_burn_failure_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in

  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // BURN (with admin)
  let burn_request = ({ recipient=owner1; token_id=1n; amount=2n } : CMTAT_nft_asset.CmtatNftAssetExtendable.burn_param)
  in
  let r = Test.transfer orig.addr (Burn burn_request) 0tez in
  let () = string_failure r  CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_burner in
  ()

let test_burn_success_with_burner =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // BURN 
  let () = Test.set_source op2 in
  let burn_request = ({ recipient=owner1; token_id=1n; amount=1n } : CMTAT_nft_asset.CmtatNftAssetExtendable.burn_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner4, 4n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_not_owned orig.addr 1n in
  let () = assert_totalsupply orig.addr 1n 0n in
  ()

let test_burn_failure_not_burner =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // BURN (with not burner)
  let () = Test.set_source op3 in
  let burn_request = ({ recipient=owner1; token_id=1n; amount=1n } : CMTAT_nft_asset.CmtatNftAssetExtendable.burn_param)
  in
  let r = Test.transfer orig.addr (Burn burn_request) 0tez in
  let () = string_failure r  CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_burner in
  let () = assert_totalsupply orig.addr 1n 1n in
  ()

let test_burn_failure_unknown_token =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // BURN 
  let () = Test.set_source op2 in
  let burn_request = ({ recipient=owner1; token_id=6n; amount=1n } : CMTAT_nft_asset.CmtatNftAssetExtendable.burn_param)
  in
  let r = Test.transfer orig.addr (Burn burn_request) 0tez in
  let () = string_failure r  CMTAT_nft_asset.CmtatNftAssetExtendable.FA2.NFTExtendable.Errors.undefined_token in
  ()


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                              AUTHORIZATION module
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let test_grant_role_success_simple =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag)) 0tez in
  let () = assert_role addr owner1 flag in
  ()

let test_grant_role_success_with_ruler =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
    // GRANT RULER ROLE 
  let () = Test.set_source initial_storage.administration.admin in
  let flag_ruler : CMTAT_nft_asset.AUTHORIZATIONS.role = RULER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op2, flag_ruler)) 0tez in
  let () = assert_role addr op2 flag_ruler in
  // GRANT ROLE 
  let () = Test.set_source op2 in
  let flag : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag)) 0tez in
  let () = assert_role addr owner1 flag in
  ()


let test_grant_role_success_multiple =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let () = assert_role addr owner1 flag_minter in
  let () = assert_role addr owner1 flag_burner in
  ()


let test_grant_role_failure_not_ruler =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source owner2 in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let r = Test.transfer_to_contract contr (GrantRole (owner1, flag_minter)) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_ruler in
  let () = assert_not_role addr owner1 flag_minter in
  ()

let test_revoke_role_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (RevokeRole (owner1, flag_burner)) 0tez in
  let () = assert_role addr owner1 flag_minter in
  let () = assert_not_role addr owner1 flag_burner in
  ()

let test_revoke_role_success_with_ruler =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  // GRANT RULER ROLE
  let () = Test.set_source initial_storage.administration.admin in
  let flag_ruler : CMTAT_nft_asset.AUTHORIZATIONS.role = RULER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op2, flag_ruler)) 0tez in
  // GRANT ROLE with RULER
  let () = Test.set_source op2 in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  // GRANT ROLE with RULER
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  // REVOKE ROLE with RULER
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (RevokeRole (owner1, flag_burner)) 0tez in
  let () = assert_role addr owner1 flag_minter in
  let () = assert_not_role addr owner1 flag_burner in
  ()

let test_revoke_role_failure_not_ruler =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  // GRANT ROLE with admin
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  // REVOKE ROLE with not RULER - fails
  let () = Test.set_source op2 in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let r = Test.transfer_to_contract contr (RevokeRole (owner2, flag_burner)) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.not_ruler in 
  let () = assert_role addr owner1 flag_minter in
  ()

let test_revoke_role_failure_missing_role_wrong_user =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let r = Test.transfer_to_contract contr (RevokeRole (owner2, flag_burner)) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.missing_role in 
  let () = assert_role addr owner1 flag_minter in
  let () = assert_role addr owner1 flag_burner in
  ()

let test_revoke_role_failure_missing_role =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_nft_asset.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_nft_asset.AUTHORIZATIONS.role = BURNER in
  let r = Test.transfer_to_contract contr (RevokeRole (owner1, flag_burner)) 0tez in
  let () = string_failure r CMTAT_nft_asset.AUTHORIZATIONS.Errors.missing_role in 
  let () = assert_role addr owner1 flag_minter in
  ()



////////////////////////////////////////////////////////////////////////////////
//          SNAPSHOTS
////////////////////////////////////////////////////////////////////////////////

let test_schedule_snapshot_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  ()

let test_schedule_snapshot_failure_before_next_scheduled =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SCHEDULE SNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // SCHEDULE SNAPSHOT
  let snapshot_time_1 = ("2023-01-01t00:00:00Z" : timestamp) in
  let r = Test.transfer orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.before_next_scheduled in
  ()


let test_schedule_snapshot_failure_already_scheduled =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SCHEDULE SNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // SCHEDULE SNAPSHOT
  let snapshot_time_1 = ("2024-01-01t00:00:00Z" : timestamp) in
  let r = Test.transfer orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.already_scheduled in
  ()


let test_transfer_with_scheduled_snapshot_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SCHEDULE SNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 1n, 1n), (owner4, 4n, 1n), (owner4, 5n, 1n))in
  // let () = assert_totalsupply_snapshot orig.addr snapshot_time_0 30n in // totalsupply snapshot is not changed after a transfer
  ()

let test_mint_with_scheduled_snapshot_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param) in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 6n, 0n), (owner1, 6n, 0n), (owner1, 6n, 0n))in
  let () = assert_totalsupply_snapshot orig.addr snapshot_time_0 6n 0n in
  ()


let test_burn_with_scheduled_snapshot_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let () = Test.set_source op2 in
  let burn_request = ({ recipient=owner1; token_id=1n; amount=1n } : CMTAT_nft_asset.CmtatNftAssetExtendable.burn_param) in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  
  let () = assert_account_snapshot orig.addr snapshot_time_0 ((owner1, 1n, 1n), (owner1, 1n,  1n), (owner1, 1n, 1n))in
  let () = assert_totalsupply_snapshot orig.addr snapshot_time_0 1n 1n in
  ()


let test_reschedule_snapshot_success_1_element =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_0_resched = ("2024-01-01t02:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (RescheduleSnapshot (snapshot_time_0, snapshot_time_0_resched)) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0_resched in
  ()

let test_reschedule_snapshot_success_2_elements =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let snapshot_time_0_resched = ("2024-01-01t02:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (RescheduleSnapshot (snapshot_time_0, snapshot_time_0_resched)) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0_resched in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in
  ()

let test_reschedule_snapshot_success_3_elements =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let snapshot_time_2 = ("2024-01-03t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_2) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_2 in

  let snapshot_time_1_resched = ("2024-01-01t02:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (RescheduleSnapshot (snapshot_time_1, snapshot_time_1_resched)) 0tez in
  let () = check_invariant_scheduled_snapshot orig.addr in
  let () = assert_scheduled_snapshots_contains orig.addr [snapshot_time_0; snapshot_time_1_resched; snapshot_time_2] in
  let () = assert_no_scheduled_snapshot orig.addr snapshot_time_1 in
  ()


let test_reschedule_snapshot_failure_3_elements_lowerbound =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let snapshot_time_2 = ("2024-01-03t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_2) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_2 in

  let snapshot_time_1_resched = ("2023-01-01t00:00:00Z" : timestamp) in
  let r = Test.transfer orig.addr (RescheduleSnapshot (snapshot_time_1, snapshot_time_1_resched)) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.rescheduled_before_previous in
  ()

  
let test_reschedule_snapshot_failure_3_elements_upperbound =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let snapshot_time_2 = ("2024-01-03t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_2) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_2 in

  let snapshot_time_1_resched = ("2024-01-04t00:00:00Z" : timestamp) in
  let r = Test.transfer orig.addr (RescheduleSnapshot (snapshot_time_1, snapshot_time_1_resched)) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.rescheduled_after_next in
  ()

let test_reschedule_snapshot_failure_3_elements_already =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let snapshot_time_2 = ("2024-01-03t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_2) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_2 in

  let snapshot_time_1_resched = ("2024-01-03t00:00:00Z" : timestamp) in
  let r = Test.transfer orig.addr (RescheduleSnapshot (snapshot_time_1, snapshot_time_1_resched)) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.already_scheduled in
  ()

let test_unschedule_snapshot_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let () = Test.set_source initial_storage.administration.admin in
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let _r = Test.transfer_exn orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_no_scheduled_snapshot orig.addr snapshot_time_0 in
  ()

let test_unschedule_snapshot_success_with_snapshooter =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let () = Test.set_source initial_storage.administration.admin in
  let flag_snapshooter : CMTAT_nft_asset.AUTHORIZATIONS.role = SNAPSHOOTER in
  let _ = Test.transfer_exn orig.addr (GrantRole (owner1, flag_snapshooter)) 0tez in

  let () = Test.set_source owner1 in
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let _r = Test.transfer_exn orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_no_scheduled_snapshot orig.addr snapshot_time_0 in
  ()

let test_unschedule_snapshot_failure_not_found =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  let r = Test.transfer orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.snapshot_not_found in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  ()

let test_unschedule_snapshot_failure_no_scheduled =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let () = assert_no_scheduled_snapshot orig.addr snapshot_time_0 in
  let r = Test.transfer orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.no_snapshot_scheduled in
  ()

// let test_unschedule_snapshot_failure_in_past =
//   let initial_storage, owners, operators = get_initial_storage () in
//   let _owner1 = List_helper.nth_exn 0 owners in
//   let _owner2 = List_helper.nth_exn 1 owners in
//   let _owner3 = List_helper.nth_exn 2 owners in
//   let op1    = List_helper.nth_exn 0 operators in
//   let () = Test.set_source op1 in
//   let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

//   let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
//   let () = assert_no_scheduled_snapshot orig.addr snapshot_time_0 in
//   let r = Test.transfer orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
//   let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.no_snapshot_scheduled in
//   ()

let test_unschedule_snapshot_failure_in_past =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let () = Test.set_source initial_storage.administration.admin in
  let flag_snapshooter : CMTAT_nft_asset.AUTHORIZATIONS.role = SNAPSHOOTER in
  let _ = Test.transfer_exn orig.addr (GrantRole (owner1, flag_snapshooter)) 0tez in

  let () = Test.set_source owner1 in
  let snapshot_time_0 = ("1970-01-01t00:30:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in

  let () = Test.bake_until_n_cycle_end 1n in

  let r = Test.transfer orig.addr (UnscheduleSnapshot snapshot_time_0) 0tez in
  let () = string_failure r CMTAT_nft_asset.SNAPSHOTS.Errors.snapshot_already_done in
  ()


/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        SNAPSHOT  VIEWS
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_getnextsnapshots_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_GETNEXTSNAPSHOTS) ([] : timestamp list) 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in
  // SCHEDULESNAPSHOT
  let snapshot_time_1 = ("2024-01-02t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_1) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_1 in

  // Call View of Caller contract
 // Caller contract calls the "getNextSnapshots" view of CMTAT contract
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = [snapshot_time_0; snapshot_time_1]) in
  ()


let test_snapshot_totalsupply_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTTOTALSUPPLY) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in


  // Keep in mind the totalsupply before mint
  let storage = Test.get_storage orig.addr in
  let totalsupply_before_mint = match Big_map.find_opt 6n storage.totalsupplies with
  | None -> 0n
  | Some (v) -> v
  in

  // MINT
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 6n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_totalsupply orig.addr 6n 1n in

  // Call View of Caller contract
 // Caller contract calls the "snapshotTotalSupply" view of CMTAT contract
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, 6n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = totalsupply_before_mint) in
  ()


let test_snapshot_balanceof_view_success =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION CALLER
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller_SNAPSHOTBALANCEOF) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 

  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  let contr = Test.to_contract orig.addr in 
  let fa2_address : address = Tezos.address contr in

  // SCHEDULESNAPSHOT
  let snapshot_time_0 = ("2024-01-01t00:00:00Z" : timestamp) in
  let _r = Test.transfer_exn orig.addr (ScheduleSnapshot snapshot_time_0) 0tez in
  let () = assert_scheduled_snapshot orig.addr snapshot_time_0 in


  // Keep in mind the owner1 balance before mint
  let storage = Test.get_storage orig.addr in
  let token_6_total_supply = match Big_map.find_opt 6n storage.ledger with
    Some _amt -> 1n
  | None -> 0n
  in

  // MINT
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=6n; token_info={token_id=6n;token_info=(Map.empty : (string, bytes) map)} } : CMTAT_nft_asset.CmtatNftAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 6n), (owner2, 2n), (owner3, 3n)) in
  let () = assert_totalsupply orig.addr 6n 1n in

  // Call View of Caller contract
 // Caller contract calls the "snapshotBalanceOf" view of CMTAT contract
  let _ = Test.transfer_to_contract_exn contr_caller (Request (fa2_address, snapshot_time_0, owner1, 6n)) 0tez in
  let storage_caller = Test.get_storage orig_caller.addr in
  let () = assert(storage_caller = token_6_total_supply) in
  ()



////////////////////////////////////////////////////////////////////////////////
//          VALIDATION (RULE_ENGINE_BLACKLIST)
////////////////////////////////////////////////////////////////////////////////

let test_setruleengine_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_BLACKLIST.storage = {
    admin = op1;
    frozens = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_BLACKLIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in

  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  ()


let test_transfer_failure_because_refused =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_BLACKLIST.storage = {
    admin = op1;
    frozens = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_BLACKLIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
  // FREEZE on RULE_ENGINE
  let _ = Test.transfer_exn orig_caller.addr (Freeze owner1) 0tez in

  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.VALIDATION.Errors.refused_by_rule_engine in
  ()


let test_transfer_success_without_frozen =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_BLACKLIST.storage = {
    admin = op1;
    frozens = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_BLACKLIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
  // FREEZE on RULE_ENGINE
  let _ = Test.transfer_exn orig_caller.addr (Freeze owner1) 0tez in

  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner2; txs=([{to_=owner3;token_id=2n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n), (owner3, 2n), (owner3, 3n)) in
  ()


let test_transfer_failure_invalid_rule_engine =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
 
  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.VALIDATION.Errors.invalid_rule_engine in
  ()


////////////////////////////////////////////////////////////////////////////////
//          VALIDATION (RULE_ENGINE_WHITELIST)
////////////////////////////////////////////////////////////////////////////////

let test_setruleengine_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage () in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_WHITELIST.storage = {
    admin = op1;
    authorized = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_WHITELIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in

  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  ()


let test_transfer_failure_because_refused =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_WHITELIST.storage = {
    admin = op1;
    authorized = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_WHITELIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
  // FREEZE on RULE_ENGINE
  let _ = Test.transfer_exn orig_caller.addr (Freeze owner1) 0tez in

  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.VALIDATION.Errors.refused_by_rule_engine in
  ()


let test_transfer_success_without_frozen =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let init_store_rule_engine : RULE_ENGINE_WHITELIST.storage = {
    admin = op1;
    authorized = Big_map.empty
  } in
  let orig_caller = Test.originate (contract_of RULE_ENGINE_WHITELIST) init_store_rule_engine 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
  // FREEZE on RULE_ENGINE
  let _ = Test.transfer_exn orig_caller.addr (Unfreeze owner2) 0tez in
  let _ = Test.transfer_exn orig_caller.addr (Unfreeze owner3) 0tez in

  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner2; txs=([{to_=owner3;token_id=2n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n), (owner3, 2n), (owner3, 3n)) in
  ()


let test_transfer_failure_invalid_rule_engine =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
	let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  // ORIGINATION RULE_ENGINE
  let () = Test.set_source op1 in
  let orig_caller = Test.originate (contract_of Caller) 0n 0tez in
  let contr_caller = Test.to_contract orig_caller.addr in 
  let rule_engine_address : address = Tezos.address contr_caller in
 
  // ORIGINATION 
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_nft_asset) initial_storage 0tez in
  // SET RULE_ENGINE
  let _ = Test.transfer_exn orig.addr (SetRuleEngine (Some(rule_engine_address))) 0tez in
  let () = assert_rule_engine orig.addr (Some(rule_engine_address)) in
  // TRANSFER
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=1n;amount=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner4; txs=([{to_=owner3;token_id=4n;amount=1n};{to_=owner1;token_id=5n;amount=1n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_nft_asset.VALIDATION.Errors.invalid_rule_engine in
  ()
