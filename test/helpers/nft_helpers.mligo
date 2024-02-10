#import "../cmtat/extended_cmtat_nft_asset.instance.mligo" "CMTAT_nft_asset"

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
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.Token.TZIP12.tokenMetadataData));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.Token.TZIP12.tokenMetadataData));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.Token.TZIP12.tokenMetadataData));
    (4n, ({token_id=4n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.Token.TZIP12.tokenMetadataData));
    (5n, ({token_id=5n;token_info=(Map.empty : (string, bytes) map);} : CMTAT_nft_asset.Token.TZIP12.tokenMetadataData));
  ] : CMTAT_nft_asset.Token.TZIP12.tokenMetadata) in

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

  let minter_role : CMTAT_nft_asset.Token.AUTHORIZATIONS.role = MINTER in
  let burner_role : CMTAT_nft_asset.Token.AUTHORIZATIONS.role = BURNER in
  let profil_minter : CMTAT_nft_asset.Token.AUTHORIZATIONS.role set =  Set.add minter_role Set.empty in
  let profil_burner : CMTAT_nft_asset.Token.AUTHORIZATIONS.role set =  Set.add burner_role Set.empty in

  let initial_storage : CMTAT_nft_asset.storage = {
    // ledger         = ledger;
    // token_metadata = token_metadata;
    // operators      = operators;
    // metadata       = metadata;

    ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      administration = { admin = op1; paused = false; killed = false };
      totalsupplies  = Big_map.literal([(1n, 1n); (2n, 1n); (3n, 1n); (4n, 1n); (5n, 1n)]);
      authorizations = Big_map.literal([(op2, profil_burner); (op3, profil_minter)]);
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

let assert_error (result : test_exec_result) (error : CMTAT_nft_asset.Token.FA2.NFTExtendable.Errors.t) =
  match result with
    Success _ -> Test.failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval error))
  | Fail _ -> Test.failwith "invalid test failure"
