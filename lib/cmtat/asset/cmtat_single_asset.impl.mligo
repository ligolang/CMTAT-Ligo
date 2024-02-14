
#import "./extendable_cmtat_single_asset.impl.mligo" "CmtatSingleAssetExtendable"
#import "../modules/administration.mligo" "ADMINISTRATION"
#import "../modules/single_asset/totalsupply.mligo" "TOTALSUPPLY"
#import "../modules/single_asset/authorizations.mligo" "AUTHORIZATIONS"
#import "../modules/single_asset/snapshots.mligo" "SNAPSHOTS"
#import "../modules/validation.mligo" "VALIDATION"

type ledger = CmtatSingleAssetExtendable.ledger

type operator = CmtatSingleAssetExtendable.operator

type operators = CmtatSingleAssetExtendable.operators

//TODO
// type storage = unit CmtatSingleAssetExtendable.storage

type storage =
{
  administration : ADMINISTRATION.t;
  totalsupplies: TOTALSUPPLY.t;
  authorizations: AUTHORIZATIONS.t;
  snapshots: SNAPSHOTS.t;
  validation: VALIDATION.t;
  ledger : ledger;
  operators : operators;
  token_metadata : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadata;
  metadata : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP16.metadata;
}

type ret = operation list * storage

let empty_storage (admin, paused: address * bool): storage =
  {
    administration = { admin=admin; paused=paused; killed = false };
    totalsupplies = 0n;
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
let lift (s : storage) : unit CmtatSingleAssetExtendable.storage =
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
let unlift (ret : operation list * unit CmtatSingleAssetExtendable.storage) : ret =
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
let transfer (t : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.transfer) (s : storage) : ret =
  unlift (CmtatSingleAssetExtendable.transfer t (lift s))

[@entry]
let balance_of (b : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.balance_of) (s : storage) : ret =
  unlift (CmtatSingleAssetExtendable.balance_of b (lift s))

[@entry]
let update_operators (updates : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.update_operators) (s : storage) : ret =
  unlift (CmtatSingleAssetExtendable.update_operators updates (lift s))

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  CmtatSingleAssetExtendable.get_balance p (lift s)

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  CmtatSingleAssetExtendable.total_supply token_id (lift s)

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  CmtatSingleAssetExtendable.all_tokens () (lift s)

[@view]
let is_operator (op : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.operator) (s : storage) : bool =
  CmtatSingleAssetExtendable.is_operator op (lift s)

[@view]
let token_metadata (p : nat) (s : storage) : CmtatSingleAssetExtendable.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData =
  CmtatSingleAssetExtendable.token_metadata p (lift s)

[@entry]
let pause (p: CmtatSingleAssetExtendable.ADMINISTRATION.pause_param) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.pause p (lift s))

[@entry]
let mint  (p: CmtatSingleAssetExtendable.mint_param) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.mint p (lift s))

[@entry]
let burn (p: CmtatSingleAssetExtendable.burn_param) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.burn p (lift s))

[@entry]
let grantRole (p: address * CmtatSingleAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.grantRole p (lift s))

[@entry]
let revokeRole (p: address * CmtatSingleAssetExtendable.AUTHORIZATIONS.role) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.revokeRole p (lift s))

[@entry]
let scheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.scheduleSnapshot p (lift s))

[@entry]
let rescheduleSnapshot (p: timestamp * timestamp) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.rescheduleSnapshot p (lift s))

[@entry]
let unscheduleSnapshot (p: timestamp) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.unscheduleSnapshot p (lift s))

[@entry]
let setRuleEngine (p: CmtatSingleAssetExtendable.VALIDATION.rule_engine_param) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.setRuleEngine p (lift s))

[@entry]
let kill (_p: unit) (s: storage) : ret =
  unlift (CmtatSingleAssetExtendable.kill () (lift s))

[@view]
let getNextSnapshots (_p: unit) (s: storage) : timestamp list =
  CmtatSingleAssetExtendable.getNextSnapshots (lift s)

[@view]
let snapshotTotalsupply (p: timestamp * nat) (s: storage) : nat =
  CmtatSingleAssetExtendable.snapshotTotalsupply p (lift s)

[@view]
let snapshotBalanceOf (p: timestamp * address * nat) (s: storage) : nat =
  CmtatSingleAssetExtendable.snapshotBalanceOf p (lift s)

