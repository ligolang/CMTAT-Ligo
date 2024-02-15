
#import "@ligo/fa/lib/main.mligo" "FA2"
#import "../modules/administration.mligo" "ADMINISTRATION"
#import "../modules/nft_asset/totalsupply.mligo" "TOTALSUPPLY"
#import "../modules/nft_asset/authorizations.mligo" "AUTHORIZATIONS"
#import "../modules/nft_asset/snapshots.mligo" "SNAPSHOTS"
#import "../modules/validation.mligo" "VALIDATION"

module TZIP12 = FA2.NFTExtendable.TZIP12
module TZIP16 = FA2.NFTExtendable.TZIP16

type ledger = FA2.NFTExtendable.ledger

type operator = address

type operators = FA2.NFTExtendable.operators

type 'a storage =
{
    ledger : ledger;
    operators : operators;
    token_metadata : TZIP12.tokenMetadata;
    metadata : FA2.NFTExtendable.TZIP16.metadata;
    administration : ADMINISTRATION.t;
    totalsupplies: TOTALSUPPLY.t;
    authorizations: AUTHORIZATIONS.t;
    snapshots: SNAPSHOTS.t;
    validation: VALIDATION.t;
    extension : 'a
}


type 'a ret = operation list * 'a storage

module Errors = struct
    let already_defined_token = "FA2_TOKEN_ALREADY_DEFINED"
end

let transfer (type a) (tr : TZIP12.transfer) (s : a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let () = ADMINISTRATION.assert_not_paused s.administration in
    let () = VALIDATION.assert_validateTransfer tr s.validation in
    let new_snapshots = SNAPSHOTS.update tr s.ledger s.totalsupplies s.snapshots in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.NFTExtendable.transfer tr sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
        snapshots = new_snapshots;
    }

let balance_of (type a) (b : TZIP12.balance_of) (s : a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.NFTExtendable.balance_of b sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
    }

let update_operators (type a) (updates : TZIP12.update_operators) (s : a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.NFTExtendable.update_operators updates sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
    }

let get_balance (type a) (p : (address * nat)) (s : a storage) : nat =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.NFTExtendable.get_balance p sub_fa2_storage

let total_supply (type a) (token_id : nat) (s : a storage) : nat =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    //    let sub_fa2_storage = {
    //     ledger=s.ledger; 
    //     operators=s.operators;
    //     token_metadata=s.token_metadata;
    //     metadata=s.metadata;
    //     extension=s.extension;
    // } in
    // get_total_supply_curried(s.totalsupplies, token_id)
    TOTALSUPPLY.get_total_supply s.totalsupplies token_id
    // FA2.NFTExtendable.total_supply token_id sub_fa2_storage

let all_tokens (type a) (_ : unit) (s : a storage) : nat set =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.NFTExtendable.all_tokens () sub_fa2_storage

let is_operator (type a) (op : FA2.NFTExtendable.TZIP12.operator) (s : a storage) : bool =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.NFTExtendable.is_operator op sub_fa2_storage

let token_metadata (type a) (p : nat) (s : a storage) : FA2.NFTExtendable.TZIP12.tokenMetadataData =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.NFTExtendable.token_metadata p sub_fa2_storage





let pause (type a) (p: ADMINISTRATION.pause_param) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, PAUSER) s.authorizations)) AUTHORIZATIONS.Errors.not_pauser in
    [], { s with administration = ADMINISTRATION.pause p s.administration }


// TODO
type token_id = nat

type mint_param = { 
    recipient : address;
    token_id : token_id;
    token_info : FA2.NFTExtendable.TZIP12.tokenMetadataData
}

let mint (type a)  (p: mint_param) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error (AUTHORIZATIONS.hasRole (sender, MINTER) s.authorizations) AUTHORIZATIONS.Errors.not_minter in
    let { recipient; token_id; token_info } = p in
    let new_snapshots = SNAPSHOTS.update_atomic (None, Some(recipient), 1n, token_id) s.ledger s.totalsupplies s.snapshots in
    
    // ensure token does not exist
    let new_token_metadata = match Big_map.find_opt token_id s.token_metadata with
    | None -> Big_map.add token_id token_info s.token_metadata
    | Some (_v) -> failwith Errors.already_defined_token
    in 
    // update ledger
    let new_ledger = Big_map.update token_id (Some recipient) s.ledger in
    // let new_ledger = FA2.NFTExtendable.set_balance s recipient token_id in
    // update total supply
    let new_total = TOTALSUPPLY.increase_token_total_supply s.totalsupplies token_id 1n in
    ([]: operation list), { s with 
            ledger = new_ledger; 
            totalsupplies = new_total;
            snapshots = new_snapshots;
            token_metadata = new_token_metadata;
        }

type burn_param = { 
    recipient : address;
    token_id : token_id;
    amount : nat
}

let burn (type a) (p: burn_param) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let { recipient; token_id; amount=_amount } = p in
    let sender = Tezos.get_sender() in
    let () = assert_with_error (AUTHORIZATIONS.hasRole (sender, BURNER) s.authorizations) AUTHORIZATIONS.Errors.not_burner in
    let new_snapshots = SNAPSHOTS.update_atomic (Some(recipient), None, 1n, token_id) s.ledger s.totalsupplies s.snapshots in
    // let new_ledger = FA2.NFTExtendable.decrease_token_amount_for_user s.ledger recipient token_id amount in
    // ensure token exist
    let new_token_metadata = match Big_map.find_opt token_id s.token_metadata with
    | None -> failwith FA2.NFTExtendable.Errors.undefined_token
    | Some (_v) -> Big_map.remove token_id s.token_metadata
    in 
    // update ledger
    let new_ledger = Big_map.remove token_id s.ledger in
    let new_total = TOTALSUPPLY.decrease_token_total_supply s.totalsupplies token_id 1n in
    ([]: operation list), { s with 
            ledger = new_ledger; 
            totalsupplies = new_total;
            snapshots = new_snapshots;
            token_metadata = new_token_metadata
        }

let grantRole (type a) (p: address * AUTHORIZATIONS.role) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, RULER) s.authorizations)) AUTHORIZATIONS.Errors.not_ruler in
    [], { s with authorizations = AUTHORIZATIONS.grantRole p s.authorizations }


let revokeRole (type a) (p: address * AUTHORIZATIONS.role) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, RULER) s.authorizations)) AUTHORIZATIONS.Errors.not_ruler in
    [], { s with authorizations = AUTHORIZATIONS.revokeRole p s.authorizations }


let scheduleSnapshot (type a) (p: timestamp) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, SNAPSHOOTER) s.authorizations)) AUTHORIZATIONS.Errors.not_snapshooter in
    [], { s with snapshots = SNAPSHOTS.scheduleSnapshot p s.snapshots }

let rescheduleSnapshot (type a) (p: timestamp * timestamp) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, SNAPSHOOTER) s.authorizations)) AUTHORIZATIONS.Errors.not_snapshooter in
    [], { s with snapshots = SNAPSHOTS.rescheduleSnapshot p.0 p.1 s.snapshots }

let unscheduleSnapshot (type a) (p: timestamp) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, SNAPSHOOTER) s.authorizations)) AUTHORIZATIONS.Errors.not_snapshooter in
    [], { s with snapshots = SNAPSHOTS.unscheduleSnapshot p s.snapshots }

let getNextSnapshots (type a) (s: a storage) : timestamp list =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    SNAPSHOTS.getNextSnapshots s.snapshots

let snapshotTotalsupply (type a) (p: timestamp * nat) (s: a storage) : nat =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    SNAPSHOTS.snapshotTotalsupply p.0 p.1 s.totalsupplies s.snapshots

let snapshotBalanceOf (type a) (p: timestamp * address * nat) (s: a storage) : nat =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    SNAPSHOTS.snapshotBalanceOf p.0 p.1 p.2 s.ledger s.snapshots

let setRuleEngine (type a) (p: VALIDATION.rule_engine_param) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, VALIDATOR) s.authorizations)) AUTHORIZATIONS.Errors.not_validator in
    [], { s with validation = VALIDATION.set_rule_engine p s.validation }

let kill (type a) (_p: unit) (s: a storage) : a ret =
    let () = ADMINISTRATION.assert_not_killed s.administration in
    let sender = Tezos.get_sender() in
    let () = assert_with_error (sender = s.administration.admin) ADMINISTRATION.Errors.not_admin in
    [], { s with 
      ledger         = Big_map.empty;
      metadata       = Big_map.empty;
      token_metadata = Big_map.empty;
      operators      = Big_map.empty;
      administration = { s.administration with paused = true; killed = true };
      totalsupplies  = Big_map.empty;
      authorizations = Big_map.empty;
      snapshots = {
        account_snapshots = Big_map.empty;
        totalsupply_snapshots = Map.empty;
        scheduled_snapshots = ([] : timestamp list)
      };
      validation = {
        rule_engine_contract = (None: address option)
      };
    }