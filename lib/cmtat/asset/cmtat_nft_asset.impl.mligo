
#import "./extendable_cmtat_nft_asset.impl.mligo" "CmtatNftAssetExtendable"
#import "../modules/administration.mligo" "ADMINISTRATION"
#import "../modules/nft_asset/totalsupply.mligo" "TOTALSUPPLY"
#import "../modules/nft_asset/authorizations.mligo" "AUTHORIZATIONS"
#import "../modules/nft_asset/snapshots.mligo" "SNAPSHOTS"
#import "../modules/validation.mligo" "VALIDATION"

type ledger = CmtatNftAssetExtendable.ledger

type operator = CmtatNftAssetExtendable.operator

type operators = CmtatNftAssetExtendable.operators

// type storage = unit CmtatNftAssetExtendable.storage

type storage =
{
  administration : ADMINISTRATION.t;
  totalsupplies: TOTALSUPPLY.t;
  authorizations: AUTHORIZATIONS.t;
  snapshots: SNAPSHOTS.t;
  validation: VALIDATION.t;
  ledger : ledger;
  operators : operators;
  token_metadata : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadata;
  metadata : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP16.metadata;
}

type ret = operation list * storage

let empty_storage (admin, paused: address * bool): storage =
  {
    administration = { admin=admin; paused=paused; killed = false };
    totalsupplies = Big_map.empty;
    authorizations = Big_map.empty;
    snapshots = 
    {
      account_snapshots = (Big_map.empty : (address, SNAPSHOTS.snapshots) big_map);
      totalsupply_snapshots = (Map.empty : SNAPSHOTS.snapshots);
      scheduled_snapshots = ([] : timestamp list)
    };
    validation = {
      rule_engine_contract = (None: address option)
    };
    ledger = Big_map.empty;
    operators = Big_map.empty;
    token_metadata = Big_map.empty;
    metadata = Big_map.empty
  }

[@inline]
let lift (s : storage) : unit CmtatNftAssetExtendable.storage =
{
  administration = s.administration;
  totalsupplies = s.totalsupplies;
  authorizations = s.authorizations;
  snapshots = s.snapshots;
  validation = s.validation;
  extension = ();
  ledger = s.ledger;
  operators = s.operators;
  token_metadata = s.token_metadata;
  metadata = s.metadata
}

[@inline]
let unlift (ret : operation list * unit CmtatNftAssetExtendable.storage) : ret =
  let ops, s = ret in
  ops,
  {
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata;
   administration = s.administration;
   totalsupplies = s.totalsupplies;
   authorizations = s.authorizations;
   snapshots = s.snapshots;
   validation = s.validation;
  }

[@entry]
let transfer (t : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  unlift (CmtatNftAssetExtendable.transfer t (lift s))

[@entry]
let balance_of (b : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
  unlift (CmtatNftAssetExtendable.balance_of b (lift s))

[@entry]
let update_operators (updates : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
  unlift (CmtatNftAssetExtendable.update_operators updates (lift s))

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  CmtatNftAssetExtendable.get_balance p (lift s)

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  CmtatNftAssetExtendable.total_supply token_id (lift s)

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  CmtatNftAssetExtendable.all_tokens () (lift s)

[@view]
let is_operator (op : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
  CmtatNftAssetExtendable.is_operator op (lift s)

[@view]
let token_metadata (p : nat) (s : storage) : CmtatNftAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
  CmtatNftAssetExtendable.token_metadata p (lift s)

[@entry]
let pause (p: CmtatNftAssetExtendable.ADMINISTRATION.pause_param) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.pause p (lift s))

[@entry]
let mint  (p: CmtatNftAssetExtendable.mint_param) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.mint p (lift s))

[@entry]
let burn (p: CmtatNftAssetExtendable.burn_param) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.burn p (lift s))

[@entry]
let grantRole (p: address * CmtatNftAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.grantRole p (lift s))

[@entry]
let revokeRole (p: address * CmtatNftAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.revokeRole p (lift s))

[@entry]
let scheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.scheduleSnapshot p (lift s))

[@entry]
let rescheduleSnapshot (p: timestamp * timestamp) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.rescheduleSnapshot p (lift s))

[@entry]
let unscheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.unscheduleSnapshot p (lift s))

[@entry]
let setRuleEngine (p: CmtatNftAssetExtendable.VALIDATION.rule_engine_param) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.setRuleEngine p (lift s))

[@entry]
let kill (_p: unit) (s: storage) : ret =
  unlift (CmtatNftAssetExtendable.kill () (lift s))

[@view]
let getNextSnapshots (_p: unit) (s: storage) : timestamp list =
  CmtatNftAssetExtendable.getNextSnapshots (lift s)

[@view]
let snapshotTotalsupply (p: timestamp * nat) (s: storage) : nat =
  CmtatNftAssetExtendable.snapshotTotalsupply p (lift s)

[@view]
let snapshotBalanceOf (p: timestamp * address * nat) (s: storage) : nat =
  CmtatNftAssetExtendable.snapshotBalanceOf p (lift s)

