
#import "./extendable_cmtat_single_asset.impl.mligo" "CmtatSingleAssetExtendable"
#import "../modules/administration.mligo" "ADMINISTRATION"

type ledger = CmtatSingleAssetExtendable.ledger

type operator = CmtatSingleAssetExtendable.operator

type operators = CmtatSingleAssetExtendable.operators

//TODO
type storage = unit CmtatSingleAssetExtendable.storage

type storage =
{
    administration : ADMINISTRATION.t;
   ledger : ledger;
   operators : operators;
   token_metadata : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadata;
   metadata : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP16.metadata;
}

type ret = operation list * storage

let empty_storage (admin, paused: address * bool): storage =
  {
    administration = { admin=admin; paused=paused };
   ledger = Big_map.empty;
   operators = Big_map.empty;
   token_metadata = Big_map.empty;
   metadata = Big_map.empty
  }

[@inline]
let lift (s : storage) : unit CmtatSingleAssetExtendable.storage =
  {
    administration = s.administration;
   extension = ();
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata
  }

[@inline]
let unlift (ret : operation list * unit CmtatSingleAssetExtendable.storage) : ret =
  let ops, s = ret in
  ops,
  {
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata;
   administration = s.administration
  }

[@entry]
let transfer (t : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  unlift (CmtatSingleAssetExtendable.transfer t (lift s))

// [@entry]
// let balance_of (b : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
//   unlift (CmtatSingleAssetExtendable.balance_of b (lift s))

// [@entry]
// let update_operators (updates : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
//   unlift (CmtatSingleAssetExtendable.update_operators updates (lift s))

// [@view]
// let get_balance (p : (address * nat)) (s : storage) : nat =
//   CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.get_balance p (lift s)

// [@view]
// let total_supply (token_id : nat) (s : storage) : nat =
//   CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.total_supply token_id (lift s)

// [@view]
// let all_tokens (_ : unit) (s : storage) : nat set =
//   CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.all_tokens () (lift s)

// [@view]
// let is_operator (op : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
//   CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.is_operator op (lift s)

// [@view]
// let token_metadata (p : nat) (s : storage) : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
//   CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.token_metadata p (lift s)
