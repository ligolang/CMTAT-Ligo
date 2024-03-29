#import "extended_cmtat_nft_asset.instance.mligo" "CMTAT_nft_asset"
#import "../helpers/list.mligo" "List_helper"
#import "../helpers/nft_helpers.mligo" "TestHelpers"
#import "../helpers/balance_of_callback_contract.mligo" "Callback"

// alias
module TZIP12 = CMTAT_nft_asset.CMTAT.CMTAT_NFT_ASSET_EXTENDABLE.TZIP12

(* Tests for FA2 nft asset contract *)


type fa2_nft = (CMTAT_nft_asset parameter_of, CMTAT_nft_asset.storage) module_contract


(* Transfer *)

(* 1. transfer successful *)
let _test_atomic_transfer_operator_success (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate contract initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()

let test_atomic_transfer_operator_success = _test_atomic_transfer_operator_success (contract_of CMTAT_nft_asset)


(* 1.1. transfer successful owner *)
let _test_atomic_transfer_owner_success (contract: fa2_nft) =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source owner1 in
  let orig = Test.originate contract  initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()

let test_atomic_transfer_owner_success = _test_atomic_transfer_owner_success (contract_of CMTAT_nft_asset)


(* 2. transfer failure token undefined *)
let _test_transfer_token_undefined (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=15n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate contract  initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.undefined_token

let test_transfer_token_undefined = _test_transfer_token_undefined (contract_of CMTAT_nft_asset)


(* 3. transfer failure incorrect operator *)
let _test_atomic_transfer_failure_not_operator (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op2 in
  let orig = Test.originate contract initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.not_operator
let test_atomic_transfer_failure_not_operator

  = _test_atomic_transfer_failure_not_operator (contract_of CMTAT_nft_asset)

(* 4. self transfer *)
let _test_atomic_transfer_success_zero_amount_and_self_transfer (contract: fa2_nft) =
  let initial_storage, owners, _operators = TestHelpers.get_initial_storage () in

  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let transfer_requests = ([
    ({from_=owner2; txs=([({to_=owner2;token_id=2n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in

  let orig = Test.originate contract initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_transfer_success_zero_amount_and_self_transfer =

  _test_atomic_transfer_success_zero_amount_and_self_transfer (contract_of CMTAT_nft_asset)


(* 5. transfer failure transitive operators *)
let _test_transfer_failure_transitive_operators (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let () = Test.set_source op3 in
  let orig = Test.originate contract initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.not_operator
let test_transfer_failure_transitive_operators =

  _test_transfer_failure_transitive_operators (contract_of CMTAT_nft_asset)


(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let _test_empty_transfer_and_balance_of (contract: fa2_nft) =
  let initial_storage, _owners, _operators = TestHelpers.get_initial_storage () in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate contract initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  Test.assert (callback_storage = ([] : nat list))

let test_empty_transfer_and_balance_of = _test_empty_transfer_and_balance_of (contract_of CMTAT_nft_asset)


(* 7. balance of failure token undefined *)
let _test_balance_of_token_undefines (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate contract initial_storage 0tez in

  let result = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in
  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.undefined_token

let test_balance_of_token_undefines = _test_balance_of_token_undefines (contract_of CMTAT_nft_asset)


(* 8. duplicate balance_of requests *)
let _test_balance_of_requests_with_duplicates (contract: fa2_nft) =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=1n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
      {owner=owner1;token_id=2n};
    ] : TZIP12.request list);
    callback = callback_contract;
  } : TZIP12.balance_of) in

  let orig = Test.originate contract initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage orig_callback.addr in
  Test.assert (callback_storage = ([1n; 1n; 1n; 0n]))
let test_balance_of_requests_with_duplicates

  = _test_balance_of_requests_with_duplicates (contract_of CMTAT_nft_asset)


(* 9. 0 balance if does not hold any tokens (not in ledger) *)
let _test_balance_of_0_balance_if_address_does_not_hold_tokens (contract: fa2_nft) =
    let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let op1    = List_helper.nth_exn 0 operators in
    let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
    let callback_contract = Test.to_contract orig_callback.addr in

    let balance_of_requests = ({
      requests = ([
        {owner=owner1;token_id=1n};
        {owner=owner2;token_id=2n};
        {owner=op1;token_id=1n};
      ] : TZIP12.request list);
      callback = callback_contract;
    } : TZIP12.balance_of) in

    let orig = Test.originate contract initial_storage 0tez in

    let _ = Test.transfer_exn orig.addr (Balance_of balance_of_requests) 0tez in

    let callback_storage = Test.get_storage orig_callback.addr in
    Test.assert (callback_storage = ([1n; 1n; 0n]))
let test_balance_of_0_balance_if_address_does_not_hold_tokens =

  _test_balance_of_0_balance_if_address_does_not_hold_tokens (contract_of CMTAT_nft_asset)


(* Update operators *)

(* 10. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate contract initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 1n;
      } : TZIP12.operator) : TZIP12.unit_update)
    ] : TZIP12.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.not_operator
let test_update_operator_remove_operator_and_transfer =

  _test_update_operator_remove_operator_and_transfer (contract_of CMTAT_nft_asset)


(* 10.1. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer1 (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate contract initial_storage 0tez in


  let () = Test.set_source owner4 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner4;
        operator = op1;
        token_id = 4n;
      } : TZIP12.operator) : TZIP12.unit_update)
    ] : TZIP12.update_operators)) 0tez in

  let storage = Test.get_storage orig.addr in
  let operator_tokens = Big_map.find_opt (owner4,op1) storage.operators in
  let operator_tokens = Option.unopt operator_tokens in
  Test.assert (operator_tokens = Set.literal [5n])
let test_update_operator_remove_operator_and_transfer1 =

  _test_update_operator_remove_operator_and_transfer1 (contract_of CMTAT_nft_asset)



(* 11. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate contract initial_storage 0tez in


  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : TZIP12.operator) : TZIP12.unit_update);
    ] : TZIP12.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer =

  _test_update_operator_add_operator_and_transfer (contract_of CMTAT_nft_asset)


(* 11.1. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer1 (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner4 = List_helper.nth_exn 3 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate contract initial_storage 0tez in


  let () = Test.set_source owner4 in
  let _ = Test.transfer_exn orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner4;
        operator = op3;
        token_id = 4n;
      } : TZIP12.operator) : TZIP12.unit_update);
    ] : TZIP12.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner4; txs=([({to_=owner2;token_id=4n;amount=1n} : TZIP12.atomic_trans);])});
  ] : TZIP12.transfer)
  in
  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer1 =

  _test_update_operator_add_operator_and_transfer1 (contract_of CMTAT_nft_asset)


let _test_only_sender_manage_operators (contract: fa2_nft) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate contract initial_storage 0tez in


  let () = Test.set_source owner2 in
  let result = Test.transfer orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : TZIP12.operator) : TZIP12.unit_update);
    ] : TZIP12.update_operators)) 0tez in

  TestHelpers.assert_error result CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.only_sender_manage_operators


let test_only_sender_manage_operators = _test_only_sender_manage_operators (contract_of CMTAT_nft_asset)

