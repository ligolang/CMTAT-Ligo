
#import "@ligo/fa/lib/main.mligo" "FA2"
#import "../modules/administration.mligo" "ADMINISTRATION"

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
   extension : 'a
}


type 'a ret = operation list * 'a storage

let transfer (type a) (t : FA2.SingleAssetExtendable.TZIP12.transfer) (s : a storage) : a ret =
    let () = ADMINISTRATION.assert_not_paused s.administration in
    let sub_fa2_storage = {
        ledger=s.ledger; 
        operators=s.operators;
        token_metadata=s.token_metadata;
        metadata=s.metadata;
        extension=s.extension;
    } in
    let ops, new_storage = FA2.SingleAssetExtendable.transfer t sub_fa2_storage in
    ops, {s with 
        ledger = new_storage.ledger;
        operators = new_storage.operators;
        token_metadata = new_storage.token_metadata;
        metadata = new_storage.metadata;
    }

let pause (type a) (p: ADMINISTRATION.pause_param) (s: a storage) : a ret =
    let sender = Tezos.get_sender() in
    let () = assert_with_error (sender = s.administration.admin) ADMINISTRATION.Errors.not_admin in
//   let () = assert_with_error ((sender = s.extension.admin) || (AUTHORIZATIONS.hasRole (sender, PAUSER) s.extension.authorizations)) AUTHORIZATIONS.Errors.not_pauser in
    [], { s with administration = ADMINISTRATION.pause p s.administration }

// [@entry]
// let transfer (t : FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
//   unlift (FA2.SingleAssetExtendable.transfer t (lift s))
