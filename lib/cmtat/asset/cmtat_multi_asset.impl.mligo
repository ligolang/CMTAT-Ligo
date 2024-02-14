
#import "./extendable_cmtat_multi_asset.impl.mligo" "CmtatMultiAssetExtendable"
#import "../modules/administration.mligo" "ADMINISTRATION"
#import "../modules/multi_asset/totalsupply.mligo" "TOTALSUPPLY"
#import "../modules/multi_asset/authorizations.mligo" "AUTHORIZATIONS"
#import "../modules/multi_asset/snapshots.mligo" "SNAPSHOTS"
#import "../modules/validation.mligo" "VALIDATION"

type ledger = CmtatMultiAssetExtendable.ledger

type operator = CmtatMultiAssetExtendable.operator

type operators = CmtatMultiAssetExtendable.operators

// type storage = unit CmtatMultiAssetExtendable.storage

type storage =
{
  administration : ADMINISTRATION.t;
  totalsupplies: TOTALSUPPLY.t;
  authorizations: AUTHORIZATIONS.t;
  snapshots: SNAPSHOTS.t;
  validation: VALIDATION.t;
  ledger : ledger;
  operators : operators;
  token_metadata : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadata;
  metadata : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP16.metadata;
}

type ret = operation list * storage

let empty_storage (admin, paused: address * bool): storage =
  {
    administration = { admin=admin; paused=paused; killed = false };
    totalsupplies = Big_map.empty;
    authorizations = { 
        general = Big_map.empty;
        specific = Big_map.empty;
    };
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
let lift (s : storage) : unit CmtatMultiAssetExtendable.storage =
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
let unlift (ret : operation list * unit CmtatMultiAssetExtendable.storage) : ret =
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
let transfer (t : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  unlift (CmtatMultiAssetExtendable.transfer t (lift s))

[@entry]
let balance_of (b : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
  unlift (CmtatMultiAssetExtendable.balance_of b (lift s))

[@entry]
let update_operators (updates : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
  unlift (CmtatMultiAssetExtendable.update_operators updates (lift s))

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  CmtatMultiAssetExtendable.get_balance p (lift s)

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  CmtatMultiAssetExtendable.total_supply token_id (lift s)

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  CmtatMultiAssetExtendable.all_tokens () (lift s)

[@view]
let is_operator (op : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
  CmtatMultiAssetExtendable.is_operator op (lift s)

[@view]
let token_metadata (p : nat) (s : storage) : CmtatMultiAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
  CmtatMultiAssetExtendable.token_metadata p (lift s)

[@entry]
let pause (p: CmtatMultiAssetExtendable.ADMINISTRATION.pause_param) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.pause p (lift s))

[@entry]
let mint  (p: CmtatMultiAssetExtendable.mint_param) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.mint p (lift s))

[@entry]
let burn (p: CmtatMultiAssetExtendable.burn_param) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.burn p (lift s))

[@entry]
let grantRole (p: address * nat option * CmtatMultiAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.grantRole p (lift s))

[@entry]
let revokeRole (p: address * nat option * CmtatMultiAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.revokeRole p (lift s))

[@entry]
let scheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.scheduleSnapshot p (lift s))

[@entry]
let rescheduleSnapshot (p: timestamp * timestamp) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.rescheduleSnapshot p (lift s))

[@entry]
let unscheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.unscheduleSnapshot p (lift s))

[@entry]
let setRuleEngine (p: CmtatMultiAssetExtendable.VALIDATION.rule_engine_param) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.setRuleEngine p (lift s))

[@entry]
let kill (_p: unit) (s: storage) : ret =
  unlift (CmtatMultiAssetExtendable.kill () (lift s))

[@view]
let getNextSnapshots (_p: unit) (s: storage) : timestamp list =
  CmtatMultiAssetExtendable.getNextSnapshots (lift s)

[@view]
let snapshotTotalsupply (p: timestamp * nat) (s: storage) : nat =
  CmtatMultiAssetExtendable.snapshotTotalsupply p (lift s)

[@view]
let snapshotBalanceOf (p: timestamp * address * nat) (s: storage) : nat =
  CmtatMultiAssetExtendable.snapshotBalanceOf p (lift s)

