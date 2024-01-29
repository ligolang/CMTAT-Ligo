
#import "@ligo/fa/lib/main.mligo" "FA2"
#import "../modules/administration.mligo" "ADMINISTRATION"
#import "../modules/single_asset/totalsupply.mligo" "TOTALSUPPLY"
#import "../modules/authorizations.mligo" "AUTHORIZATIONS"
#import "../modules/snapshots.mligo" "SNAPSHOTS"

type ledger = FA2.SingleAssetExtendable.ledger

type operator = address

type operators = FA2.SingleAssetExtendable.operators

type 'a storage =
{
    ledger : ledger;
    operators : operators;
    token_metadata : FA2.SingleAssetExtendable.TZIP12.tokenMetadata;
    metadata : FA2.SingleAssetExtendable.TZIP16.metadata;
    administration : ADMINISTRATION.t;
    totalsupplies: TOTALSUPPLY.t;
    authorizations: AUTHORIZATIONS.t;
    snapshots: SNAPSHOTS.t;
    extension : 'a
}


type 'a ret = operation list * 'a storage

let transfer (type a) (tr : FA2.SingleAssetExtendable.TZIP12.transfer) (s : a storage) : a ret =
    let () = ADMINISTRATION.assert_not_paused s.administration in
    let new_snapshots = SNAPSHOTS.update tr s.ledger s.totalsupplies s.snapshots in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.SingleAssetExtendable.transfer tr sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
        snapshots = new_snapshots;
    }

let balance_of (type a) (b : FA2.SingleAssetExtendable.TZIP12.balance_of) (s : a storage) : a ret =
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.SingleAssetExtendable.balance_of b sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
    }

let update_operators (type a) (updates : FA2.SingleAssetExtendable.TZIP12.update_operators) (s : a storage) : a ret =
   let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.SingleAssetExtendable.update_operators updates sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
    }

let get_balance (type a) (p : (address * nat)) (s : a storage) : nat =
       let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.SingleAssetExtendable.get_balance p sub_fa2_storage

let total_supply (type a) (_token_id : nat) (s : a storage) : nat =
    //    let sub_fa2_storage = {
    //     ledger=s.ledger; 
    //     operators=s.operators;
    //     token_metadata=s.token_metadata;
    //     metadata=s.metadata;
    //     extension=s.extension;
    // } in
    TOTALSUPPLY.get_total_supply s.totalsupplies
    // FA2.SingleAssetExtendable.total_supply token_id sub_fa2_storage

let all_tokens (type a) (_ : unit) (s : a storage) : nat set =
       let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.SingleAssetExtendable.all_tokens () sub_fa2_storage

let is_operator (type a) (op : FA2.SingleAssetExtendable.TZIP12.operator) (s : a storage) : bool =
       let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.SingleAssetExtendable.is_operator op sub_fa2_storage

let token_metadata (type a) (p : nat) (s : a storage) : FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
       let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    FA2.SingleAssetExtendable.token_metadata p sub_fa2_storage





let pause (type a) (p: ADMINISTRATION.pause_param) (s: a storage) : a ret =
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, PAUSER) s.authorizations)) AUTHORIZATIONS.Errors.not_pauser in
    [], { s with administration = ADMINISTRATION.pause p s.administration }


// TODO
type token_id = nat

type mint_param = { 
    recipient : address;
    token_id : token_id;
    amount : nat
}

let mint (type a)  (p: mint_param) (s: a storage) : a ret =
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, MINTER) s.authorizations)) AUTHORIZATIONS.Errors.not_minter in
    let { recipient; token_id; amount } = p in
    let new_snapshots = SNAPSHOTS.update_atomic (None, Some(recipient), amount, token_id) s.ledger s.totalsupplies s.snapshots in
    let new_ledger = FA2.SingleAssetExtendable.increase_token_amount_for_user s.ledger recipient amount in
    let new_total = TOTALSUPPLY.increase_token_total_supply s.totalsupplies amount in
    ([]: operation list), { s with 
            ledger = new_ledger; 
            totalsupplies = new_total;
            snapshots = new_snapshots
        }

type burn_param = { 
    recipient : address;
    token_id : token_id;
    amount : nat
}

let burn (type a) (p: burn_param) (s: a storage) : a ret =
    let { recipient; token_id; amount } = p in
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, BURNER) s.authorizations)) AUTHORIZATIONS.Errors.not_burner in
    let new_snapshots = SNAPSHOTS.update_atomic (Some(recipient), None, amount, token_id) s.ledger s.totalsupplies s.snapshots in
    let new_ledger = FA2.SingleAssetExtendable.decrease_token_amount_for_user s.ledger recipient amount in
    let new_total = TOTALSUPPLY.decrease_token_total_supply s.totalsupplies amount in
    ([]: operation list), { s with 
            ledger = new_ledger; 
            totalsupplies = new_total;
            snapshots = new_snapshots
        }

let grantRole (type a) (p: address * AUTHORIZATIONS.role) (s: a storage) : a ret =
  let () = assert_with_error (Tezos.get_sender() = s.administration.admin) ADMINISTRATION.Errors.not_admin in
  [], { s with authorizations = AUTHORIZATIONS.grantRole p s.authorizations }


let revokeRole (type a) (p: address * AUTHORIZATIONS.role) (s: a storage) : a ret =
  let () = assert_with_error (Tezos.get_sender() = s.administration.admin) ADMINISTRATION.Errors.not_admin in
  [], { s with authorizations = AUTHORIZATIONS.revokeRole p s.authorizations }


let scheduleSnapshot (type a) (p: timestamp) (s: a storage) : a ret =
    let sender = Tezos.get_sender() in
    let () = assert_with_error ((sender = s.administration.admin) || (AUTHORIZATIONS.hasRole (sender, SNAPSHOOTER) s.authorizations)) AUTHORIZATIONS.Errors.not_snapshooter in
    [], { s with snapshots = SNAPSHOTS.scheduleSnapshot p s.snapshots }
