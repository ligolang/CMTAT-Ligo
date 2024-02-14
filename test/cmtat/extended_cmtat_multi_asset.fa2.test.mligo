#import "extended_cmtat_multi_asset.instance.mligo" "CMTAT_multi_asset"
#import "../helpers/list.mligo" "List_helper"
#import "../helpers/balance_of_callback_contract.mligo" "Callback"

// alias
module TZIP12 = CMTAT_multi_asset.CMTAT.CMTAT_MULTI_ASSET_EXTENDABLE.TZIP12

(* Tests for FA2 multi asset contract *)
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


  let initial_storage : CMTAT_multi_asset.storage  = {
      ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      administration = { admin = op1; paused = false; killed = false };
      totalsupplies  = Big_map.literal([(0n, a + b + c)]);
      authorizations = {
        general = Big_map.empty;
        specific = Big_map.empty;
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


(* Transfer *)

(* 1. transfer successful *)
let test_atomic_transfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 2n, 8n), (owner2, 2n, 12n), (owner3, 3n, 10n)) in
  ()

(* 2. transfer failure token undefined *)
let test_transfer_token_undefined =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=2n;token_id=1n};{to_=owner3;amount=3n;token_id=2n}] : TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;amount=2n;token_id=0n};{to_=owner1;amount=3n;token_id=2n}] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.undefined_token))
  | Fail _ -> failwith "invalid test failure"

(* 3. transfer failure incorrect operator *)
let test_atomic_transfer_failure_not_operator =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op3 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 4. transfer failure insuffient balance *)
let test_atomic_transfer_failure_not_suffient_balance =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=12n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.ins_balance))
  | Fail _ -> failwith "invalid test failure"

(* 5. transfer successful 0 amount & self transfer *)
let test_atomic_transfer_success_zero_amount_and_self_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=0n;token_id=1n};{to_=owner3;amount=0n;token_id=1n}] : TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 1n, 10n), (owner2, 2n, 10n), (owner3, 3n, 10n)) in
  ()

(* 6. transfer failure transitive operators *)
let test_transfer_failure_transitive_operators =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner3; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op3 in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* Balance of *)

(* 7. empty balance of + callback with empty response *)
let test_empty_transfer_and_balance_of =
  let initial_storage, _owners, _operators = get_initial_storage (10n, 10n, 10n) in
  let orig_callback= Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  assert (callback_storage = ([] : nat list))

(* 8. balance of failure token undefined *)
let test_balance_of_token_undefines =
  let initial_storage, owners, _operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let orig_callback= Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let result = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in

  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.undefined_token))
  | Fail _ -> failwith "invalid test failure"

(* 9. duplicate balance_of requests *)
let test_balance_of_requests_with_duplicates =
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3= List_helper.nth_exn 2 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let orig_callback= Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=1n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  assert (callback_storage = ([10n; 5n; 10n]))

(* 10. 0 balance if does not hold any tokens (not in ledger) *)
let test_balance_of_0_balance_if_address_does_not_hold_tokens =
    let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let _owner3= List_helper.nth_exn 2 owners in
    let op1    = List_helper.nth_exn 0 operators in
    let orig_callback= Test.originate (contract_of Callback) ([] : nat list) 0tez in
    let callback_contract = Test.to_contract orig_callback.addr in

    let balance_of_requests = ({
      requests = ([
        {owner=owner1;token_id=1n};
        {owner=owner2;token_id=2n};
        {owner=op1;token_id=1n};
      ] : TZIP12.request list);
      callback = callback_contract;
    } : TZIP12.balance_of) in

    let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in

    let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

    let callback_storage = Test.get_storage orig_callback.addr in
    assert (callback_storage = ([10n; 5n; 0n]))


(* Update operators *)

(* 11. Remove operator & do transfer - failure *)
let test_update_operator_remove_operator_and_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3= List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 2n;
      } : TZIP12.operator) : TZIP12.unit_update)
    ] : TZIP12.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  match result with
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval CMTAT_multi_asset.Token.FA2.MultiAssetExtendable.Errors.not_operator))
  | Fail _ -> failwith "invalid test failure"

(* 12. Add operator & do transfer - success *)
let test_update_operator_add_operator_and_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3= List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate (contract_of CMTAT_multi_asset) initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 2n;
      } : TZIP12.operator) : TZIP12.unit_update);
    ] : TZIP12.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;amount=2n;token_id=2n};] : TZIP12.atomic_trans list)});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  ()
