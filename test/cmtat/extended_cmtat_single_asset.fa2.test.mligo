#import "extended_cmtat_single_asset.instance.mligo" "CMTAT_single_asset"
#import "../helpers/list.mligo" "List_helper"
#import "../helpers/balance_of_callback_contract.mligo" "Callback"


let get_initial_storage (a, b, c : nat * nat * nat) =
  let () = Test.reset_state 6n ([] : tez list) in

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
      administration = { admin = op1; paused = false };
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

// let assert_paused
//   (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
//   (expected : bool) =
//   let storage = Test.get_storage contract_address in
//   let administration = storage.administration in
//   assert (administration.paused = expected)

let assert_totalsupply
  (contract_address : (CMTAT_single_asset parameter_of, CMTAT_single_asset.storage) typed_address )
  (expected : nat) =
  let storage = Test.get_storage contract_address in
  let actual = storage.totalsupplies in
  assert (actual = expected)

// let assert_role
//   (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
//   (user : address)
//   (expected_role: CMTAT_single_asset.Token.AUTHORIZATIONS.role) =
//     let storage = Test.get_storage contract_address in
//     match Big_map.find_opt user storage.authorizations with
//     | Some(flags) -> assert (Set.mem expected_role flags)
//     | None -> failwith "[assert_role] Unknown user"

// let assert_not_role
//   (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
//   (user : address)
//   (expected_role: CMTAT_single_asset.Token.AUTHORIZATIONS.role) =
//     let storage = Test.get_storage contract_address in
//     match Big_map.find_opt user storage.authorizations with
//     | Some(flags) -> assert (not (Set.mem expected_role flags))
//     | None -> failwith "[assert_not_role] Unknown user"

// let assert_no_role
//   (contract_address : ((CMTAT_single_asset parameter_of), CMTAT_single_asset.storage) typed_address )
//   (user : address) =
//     let storage = Test.get_storage contract_address in
//     match Big_map.find_opt user storage.authorizations with
//     | Some(_flags) -> failwith "[assert_no_role] User should not have role"
//     | None -> ()

// (* Assert contract call results in failwith with given string *)
// let string_failure (res : test_exec_result) (expected : string) : unit =
//     let expected = Test.eval expected in
//     match res with
//         | Fail (Rejected (actual,_)) -> assert (actual = expected)
//         | Fail (Balance_too_low _err) -> Test.failwith "contract failed: balance too low"
//         | Fail (Other s) -> Test.failwith s
//         | Success _ -> Test.failwith "Transaction should fail"


/////////////////////////////////////////////////////////////////////////////////////////////////////
//            ORIGINATION
/////////////////////////////////////////////////////////////////////////////////////////////////////

let test_origination_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let () = assert_balances orig.addr ((owner1, 10n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 30n in
  ()

(* Transfer *)

(* 1. transfer successful *)
let test_atomic_transfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 8n), (owner2, 7n), (owner3, 15n)) in
  ()


(* 2. transfer failure incorrect operator *)
let test_atomic_transfer_failure_not_operator =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op3 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_single_asset.Token.FA2.SingleAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 3. transfer failure insuffient balance *)
let test_atomic_transfer_failure_not_suffient_balance =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=20n};{to_=owner3;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_single_asset.Token.FA2.SingleAssetExtendable.Errors.ins_balance))
  | Fail _ -> failwith "invalid test failure"

(* 4. transfer successful 0 amount & self transfer *)
let test_atomic_transfer_success_zero_amount_and_self_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=0n};{to_=owner3;token_id=0n;amount=0n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner2;token_id=0n;amount=2n};] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 10n), (owner2, 10n), (owner3, 10n)) in
  ()

(* 5. transfer failure transitive operators *)
let test_transfer_failure_transitive_operators =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let _owner1= List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
      ({from_=owner3; txs=([{to_=owner2;token_id=0n;amount=2n};] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op2 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_single_asset.Token.FA2.SingleAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let test_empty_transfer_and_balance_of =
  let initial_storage, _owners, _operators = get_initial_storage (10n, 10n, 10n) in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in

  let balance_of_requests = ({
    requests = ([] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.request list);
    callback = Test.to_contract orig_callback.addr;
  } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  assert (callback_storage = ([] : nat list))

(* 7. duplicate balance_of requests *)
let test_balance_of_requests_with_duplicates =
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3= List_helper.nth_exn 2 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in


  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=0n};
      {owner=owner1;token_id=0n};
    ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.request list);
    callback = Test.to_contract orig_callback.addr;
  } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  assert (callback_storage = ([10n; 5n; 10n]))

(* 8. 0 balance if does not hold any tokens (not in ledger) *)
let test_balance_of_0_balance_if_address_does_not_hold_tokens =
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3= List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in


  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=0n};
      {owner=op1;token_id=0n};
    ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.request list);
    callback = Test.to_contract orig_callback.addr;
  } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  assert (callback_storage = ([10n; 5n; 0n]))

(* Update operators *)

(* 9. Remove operator & do transfer - failure *)
let test_update_operator_remove_operator_and_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 0n;
      } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.operator) : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.unit_update)
    ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=0n};{to_=owner3;token_id=0n;amount=0n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner2;token_id=0n;amount=2n};] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_single_asset.Token.FA2.SingleAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 10. Add operator & do transfer - success *)
let test_update_operator_add_operator_and_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 0n;
      } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.operator) : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in

  let () = Test.set_source owner2 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner2;
        operator = op3;
        token_id = 0n;
      } : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.operator) : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=0n};{to_=owner3;token_id=0n;amount=0n}] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner2;token_id=0n;amount=2n};] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  ()
