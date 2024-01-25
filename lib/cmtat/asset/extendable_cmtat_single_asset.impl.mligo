
#import "@ligo/fa/lib/main.mligo" "FA2"

type ledger = FA2.SingleAssetExtendable.ledger

type operator = address

type operators = FA2.SingleAssetExtendable.operators

type 'a storage =
{
   ledger : ledger;
   operators : operators;
   token_metadata : FA2.NFTExtendable.TZIP12.tokenMetadata;
   metadata : FA2.NFTExtendable.TZIP16.metadata;
   extension : 'a;
}


type 'a ret = operation list * 'a storage

let transfer (type a) (t : FA2.SingleAssetExtendable.TZIP12.transfer) (s : a storage) : a ret =
    FA2.SingleAssetExtendable.transfer t s

// [@entry]
// let transfer (t : FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
//   unlift (FA2.SingleAssetExtendable.transfer t (lift s))
