#import "extended_cmtat_single_asset.instance.mligo" "CMTAT_single_asset"
#import "../helpers/list.mligo" "List_helper"


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

//////////////////////////////////////////////////////////////////////////////////////////////
//                PAUSE (overide)
//////////////////////////////////////////////////////////////////////////////////////////////

let test_pause_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  ()

let test_pause_success_with_issuer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op2    = List_helper.nth_exn 1 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let () = Test.set_source op2 in
  let _ = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  ()

let test_pause_failure =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let () = Test.set_source op3 in
  let r = Test.transfer orig.addr (Pause true) 0tez in
  let () = string_failure r CMTAT_single_asset.Errors.not_issuer_nor_admin in
  let () = assert_paused orig.addr false in
  ()

// verify that override works. Pauser role is not enough in this extended contract , only issuer or admin)
let test_pause_failure_with_pauser =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let _owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let _op2    = List_helper.nth_exn 1 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // GRANT op3 the role Pauser
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = PAUSER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in
  // PAUSE (by op3 fails)
  let () = Test.set_source op3 in
  let r = Test.transfer orig.addr (Pause true) 0tez in
  let () = string_failure r CMTAT_single_asset.Errors.not_issuer_nor_admin in
  let () = assert_paused orig.addr false in
  ()


/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        TRANSFER
/////////////////////////////////////////////////////////////////////////////////////////////////////

let test_atomic_transfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

  let _ = Test.transfer_exn orig.addr (Transfer transfer_requests) 0tez in
  let () = assert_balances orig.addr ((owner1, 8n), (owner2, 7n), (owner3, 15n)) in
  ()

let test_atomic_transfer_failure_in_pause =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let _ = Test.transfer_exn orig.addr (Pause true) 0tez in
  let () = assert_paused orig.addr true in
  let r = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = string_failure r CMTAT_single_asset.Token.ADMINISTRATION.Errors.contract_in_pause in
  ()


/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        MINT
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_mint_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // MINT (with admin)
  let () = Test.set_source initial_storage.administration.admin in
  let mint_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let _ = Test.transfer_exn orig.addr (Mint mint_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 12n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 32n in
  ()

let test_mint_success_with_minter =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in

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
  ()

let test_mint_failure_not_minter =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  // ORIGINATION
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // MINT (fails with not minter)
  let () = Test.set_source op3 in
  let mint_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.mint_param)
  in
  let r = Test.transfer orig.addr (Mint mint_request) 0tez in
  let () = string_failure r CMTAT_single_asset.Token.AUTHORIZATIONS.Errors.not_minter in
  let () = assert_balances orig.addr ((owner1, 10n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 30n in
  ()

/////////////////////////////////////////////////////////////////////////////////////////////////////
//                        BURN
/////////////////////////////////////////////////////////////////////////////////////////////////////
let test_burn_success_with_admin =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // BURN (with admin)
  let burn_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.burn_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 8n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 28n in
  ()

let test_burn_success_with_burner =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // GRANT op3 the role Burner
  let contr = Test.to_contract orig.addr in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (op3, flag)) 0tez in
  let () = assert_role orig.addr op3 flag in
  // BURN (with burner)
  let () = Test.set_source op3 in
  let burn_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.burn_param)
  in
  let _ = Test.transfer_exn orig.addr (Burn burn_request) 0tez in
  let () = assert_balances orig.addr ((owner1, 8n), (owner2, 10n), (owner3, 10n)) in
  let () = assert_totalsupply orig.addr 28n in
  ()

let test_burn_failure_not_burner =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let op3    = List_helper.nth_exn 2 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // BURN (with burner)
  let () = Test.set_source op3 in
  let burn_request = ({ recipient=owner1; token_id=0n; amount=2n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.burn_param)
  in
  let r = Test.transfer orig.addr (Burn burn_request) 0tez in
  let () = string_failure r  CMTAT_single_asset.Token.AUTHORIZATIONS.Errors.not_burner in
  let () = assert_totalsupply orig.addr 30n in
  ()

let test_burn_failure_not_enough =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  // BURN (with admin)
  let () = Test.set_source op1 in
  let burn_request = ({ recipient=owner1; token_id=0n; amount=12n } : CMTAT_single_asset.CMTAT.CMTAT_SINGLE_ASSET.CmtatSingleAssetExtendable.burn_param)
  in
  let r = Test.transfer orig.addr (Burn burn_request) 0tez in
  let () = string_failure r  CMTAT_single_asset.Token.FA2.SingleAssetExtendable.Errors.ins_balance in
  // let () = string_failure r  CMTAT_single_asset.Token.TOTALSUPPLY.Errors.not_enough_supply in
  let () = assert_totalsupply orig.addr 30n in
  ()


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                              AUTHORIZATION module
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let test_grant_role_success_simple =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag)) 0tez in
  let () = assert_role addr owner1 flag in
  ()

let test_grant_role_success_multiple =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let () = assert_role addr owner1 flag_minter in
  let () = assert_role addr owner1 flag_burner in
  ()


let test_grant_role_failure_not_admin =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source owner2 in
  let flag_minter : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let r = Test.transfer_to_contract contr (GrantRole (owner1, flag_minter)) 0tez in
  let () = string_failure r CMTAT_single_asset.Token.ADMINISTRATION.Errors.not_admin in
  let () = assert_no_role addr owner1 in
  ()

let test_revoke_role_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (RevokeRole (owner1, flag_burner)) 0tez in
  let () = assert_role addr owner1 flag_minter in
  let () = assert_not_role addr owner1 flag_burner in
  ()

let test_revoke_role_failure_unknown_user =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_burner)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let r = Test.transfer_to_contract contr (RevokeRole (owner2, flag_burner)) 0tez in
  let () = string_failure r CMTAT_single_asset.Token.AUTHORIZATIONS.Errors.unknown_user in 
  let () = assert_role addr owner1 flag_minter in
  let () = assert_role addr owner1 flag_burner in
  ()

let test_revoke_role_failure_missing_role =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let () = Test.set_source op1 in
  let { addr;code = _code; size = _size}  = Test.originate (contract_of CMTAT_single_asset) initial_storage 0tez in
  let contr = Test.to_contract addr in
  let () = Test.set_source initial_storage.administration.admin in
  let flag_minter : CMTAT_single_asset.Token.AUTHORIZATIONS.role = MINTER in
  let _ = Test.transfer_to_contract_exn contr (GrantRole (owner1, flag_minter)) 0tez in
  let flag_burner : CMTAT_single_asset.Token.AUTHORIZATIONS.role = BURNER in
  let r = Test.transfer_to_contract contr (RevokeRole (owner1, flag_burner)) 0tez in
  let () = string_failure r CMTAT_single_asset.Token.AUTHORIZATIONS.Errors.missing_role in 
  let () = assert_role addr owner1 flag_minter in
  ()
