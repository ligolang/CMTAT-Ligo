#import "../../lib/main.mligo" "CMTAT"

module Token = CMTAT.CMTAT_SINGLE_ASSET

type storage = Token.storage

type ret = Token.ret

[@inline]
let lift (s : storage) : unit Token.CmtatSingleAssetExtendable.storage =
  {
   extension = ();
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata;
   administration = s.administration
  }

[@inline]
let unlift (ret : operation list * unit Token.CmtatSingleAssetExtendable.storage) : ret =
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
let transfer (t : Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  unlift (Token.CmtatSingleAssetExtendable.transfer t (lift s))
  

// [@entry]
// let balance_of (b : Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
//   unlift (Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.balance_of b (lift s))

// [@entry]
// let update_operators (updates : Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
//   unlift (Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.update_operators updates (lift s))

// [@view]
// let get_balance (p : (address * nat)) (s : storage) : nat =
//   Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.get_balance p (lift s)

// [@view]
// let total_supply (token_id : nat) (s : storage) : nat =
//   Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.total_supply token_id (lift s)

// [@view]
// let all_tokens (_ : unit) (s : storage) : nat set =
//   Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.all_tokens () (lift s)

// [@view]
// let is_operator (op : Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
//   Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.is_operator op (lift s)

// [@view]
// let token_metadata (p : nat) (s : storage) : Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
//   Token.CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.token_metadata p (lift s)

[@entry]
let pause (t : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
  unlift (Token.CmtatSingleAssetExtendable.pause t (lift s))