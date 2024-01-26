#import "../../lib/main.mligo" "CMTAT"

module Token = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE

type extension = {
  issuer : address
}

type storage = extension Token.storage

type ret = extension Token.ret

module Errors = struct
  let not_issuer_nor_admin = "Not issuer not admin"
end

[@entry]
let transfer (t : Token.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  Token.transfer t s
  
// [@entry]
// let balance_of (b : Token.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
//   Token.FA2.SingleAssetExtendable.balance_of b s

// [@entry]
// let update_operators (updates : Token.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
//   Token.FA2.SingleAssetExtendable.update_operators updates s

// [@view]
// let get_balance (p : (address * nat)) (s : storage) : nat =
//   Token.FA2.SingleAssetExtendable.get_balance p s

// [@view]
// let total_supply (token_id : nat) (s : storage) : nat =
//   Token.FA2.SingleAssetExtendable.total_supply token_id s

// [@view]
// let all_tokens (_ : unit) (s : storage) : nat set =
//   Token.FA2.SingleAssetExtendable.all_tokens () s

// [@view]
// let is_operator (op : Token.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
//   Token.FA2.SingleAssetExtendable.is_operator op s

// [@view]
// let token_metadata (p : nat) (s : storage) : Token.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
//   Token.FA2.SingleAssetExtendable.token_metadata p s


// [@entry]
// let pause (t : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
//   Token.pause t s

// Exemple of override of pause Entrypoint
[@entry]
let pause (p : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
  let sender = Tezos.get_sender() in
  let () = assert_with_error ((sender = s.extension.issuer) || (sender = s.administration.admin)) Errors.not_issuer_nor_admin in
  [], { s with administration = Token.ADMINISTRATION.pause p s.administration }