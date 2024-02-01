#import "../../lib/main.mligo" "CMTAT"

module Token = CMTAT.CMTAT_MULTI_ASSET_EXTENDABLE

type extension = {
  issuer : address
}

type storage = extension Token.storage

type ret = extension Token.ret

module Errors = struct
  let not_issuer_nor_admin = "Not issuer not admin"
end

[@entry]
let transfer (t : Token.FA2.MultiAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  Token.transfer t s
  
[@entry]
let balance_of (b : Token.FA2.MultiAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
  Token.balance_of b s

[@entry]
let update_operators (updates : Token.FA2.MultiAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
  Token.update_operators updates s

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  Token.get_balance p s

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  Token.total_supply token_id s

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  Token.all_tokens () s

[@view]
let is_operator (op : Token.FA2.MultiAssetExtendable.TZIP12.operator) (s : storage) : bool =
  Token.is_operator op s

[@view]
let token_metadata (p : nat) (s : storage) : Token.FA2.MultiAssetExtendable.TZIP12.tokenMetadataData =
  Token.token_metadata p s


// [@entry]
// let pause (t : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
//   Token.pause t s

// Exemple of override of pause Entrypoint
[@entry]
let pause (p : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
  let sender = Tezos.get_sender() in
  let () = assert_with_error ((sender = s.extension.issuer) || (sender = s.administration.admin)) Errors.not_issuer_nor_admin in
  [], { s with administration = Token.ADMINISTRATION.pause p s.administration }

[@entry]
let mint (p: Token.mint_param) (s: storage) : ret =
  Token.mint p s

[@entry]
let burn (p: Token.burn_param) (s: storage) : ret =
  Token.burn p s

[@entry]
let grantRole (p: address * Token.AUTHORIZATIONS.role) (s: storage) : ret =
  Token.grantRole p s

[@entry]
let revokeRole (p: address * Token.AUTHORIZATIONS.role) (s: storage) : ret =
  Token.revokeRole p s

[@entry]
let scheduleSnapshot (p: timestamp) (s: storage) : ret =
  Token.scheduleSnapshot p s

[@entry]
let rescheduleSnapshot (p: timestamp * timestamp) (s: storage) : ret =
  Token.rescheduleSnapshot p s

[@entry]
let unscheduleSnapshot (p: timestamp) (s: storage) : ret =
  Token.unscheduleSnapshot p s

[@view]
let getNextSnapshots () (s: storage) : timestamp list =
  Token.getNextSnapshots s

[@view]
let snapshotTotalsupply (p: timestamp * nat) (s: storage) : nat =
  Token.snapshotTotalsupply p s

[@view]
let snapshotBalanceOf (p: timestamp * address * nat) (s: storage) : nat =
  Token.snapshotBalanceOf p s

[@entry]
let setRuleEngine (p: Token.VALIDATION.rule_engine_param) (s: storage) : ret =
  Token.setRuleEngine p s

[@entry]
let kill (_p: unit) (s: storage) : ret =
  Token.kill () s