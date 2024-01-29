// #import "../lib/multi_asset/fa2.mligo" "FA2"
#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./single_asset/totalsupply.mligo" "TOTALSUPPLY"

module TZIP12 = FA2.SingleAssetExtendable.TZIP12


type snapshot_data = nat

type snapshots = (timestamp, snapshot_data) map

type t = {
    account_snapshots : (address, snapshots) big_map;
    totalsupply_snapshots : snapshots;
    current_snapshot_time : timestamp;
    current_snapshot_index : nat;
    scheduled_snapshots : timestamp list
}

module Errors = struct
    let schedule_in_past = "Cannot schedule in the past"
    let before_next_scheduled = "Proposed scheduled is before the next scheduled time"
    let already_scheduled = "This snapshot time is already scheduled"
end

let scheduleSnapshot (proposed : timestamp) (snapshots:t) : t =
    // check if in the past
    let () = assert_with_error (proposed > Tezos.get_now()) Errors.schedule_in_past in
    // check next scheduled
    let () = match List.head_opt snapshots.scheduled_snapshots with
    | Some next -> 
        if (proposed < next) then 
            failwith Errors.before_next_scheduled
        else if (proposed = next) then
            failwith Errors.already_scheduled
        else
            ()
    | None -> ()
    in
    let new_scheduled_snapshots = proposed :: snapshots.scheduled_snapshots in
    { snapshots with scheduled_snapshots = new_scheduled_snapshots }


let update_account_snapshot (current_scheduled_snapshot: timestamp) (account: address) (account_balance: nat) (snapshots: t) : t = 
    let new_account_snapshots = match Big_map.find_opt account snapshots.account_snapshots with
    | Some(snaps) -> 
        let new_snaps = Map.update current_scheduled_snapshot (Some(account_balance)) snaps in
        Big_map.update account (Some(new_snaps)) snapshots.account_snapshots
    | None() -> 
        let snaps = Map.literal([(current_scheduled_snapshot, account_balance)]) in
        Big_map.add account snaps snapshots.account_snapshots
    in
    { snapshots with account_snapshots = new_account_snapshots }

let update_totalsupply_snapshot (current_scheduled_snapshot: timestamp) (_token_id: nat) (totalsupply_balance: nat) (snapshots: t) : t = 
    let new_totalsupply_snapshots = Map.update current_scheduled_snapshot (Some(totalsupply_balance)) snapshots.totalsupply_snapshots in
    { snapshots with totalsupply_snapshots = new_totalsupply_snapshots }

let get_current_scheduled_snapshot (snapshots:t) : timestamp option =
    let get_current (acc, elt: timestamp option * timestamp) = match acc with
    | Some(time) -> if (Tezos.get_now() < elt) && (elt < time) then Some(elt) else acc 
    | None -> if (Tezos.get_now() < elt) then Some(elt) else acc
    in
    List.fold get_current snapshots.scheduled_snapshots (None: timestamp option)


let get_for_user_curried (ledger, owner: FA2.SingleAssetExtendable.ledger * address) = 
    FA2.SingleAssetExtendable.get_for_user ledger owner

let update_atomic (tr: address option * address option * nat * nat) (ledger: FA2.SingleAssetExtendable.ledger) (totalsupplies: TOTALSUPPLY.t) (snapshots: t) : t =
    let (from_, to_, amt, _token_id) = tr in
    if (amt = 0n) then
        snapshots
    else
        // Retrieve current scheduled time
        let current_scheduled_snapshot_opt = get_current_scheduled_snapshot snapshots in
        let new_snapshots = snapshots in
        let new_snapshots = match current_scheduled_snapshot_opt with
        | None -> snapshots
        | Some(current_scheduled_snapshot) -> 
            let new_snapshots = match (from_, to_) with
            | None, None  -> (failwith "SNAPSHOT internal error": t) // snapshots
            | None, Some(to_) -> // MINT
                // Update to_ account
                let to_balance = get_for_user_curried(ledger, to_) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot to_ to_balance new_snapshots in
                // Update total supply
                let total_supply_balance = TOTALSUPPLY.get_total_supply totalsupplies in
                let new_snapshots = update_totalsupply_snapshot current_scheduled_snapshot 0n total_supply_balance new_snapshots in
                new_snapshots
            | Some(from_), Some(to_) -> // TRANSFER
                // Update from_ account 
                let from_balance = get_for_user_curried(ledger, from_) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot from_ from_balance new_snapshots in
                // Update to_ account
                let to_balance = get_for_user_curried(ledger, to_) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot to_ to_balance new_snapshots in
                new_snapshots
            | Some(from_), None -> // BURN
                // Update from_ account 
                let from_balance = get_for_user_curried(ledger, from_) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot from_ from_balance new_snapshots in
                // Update total supply
                let total_supply_balance = TOTALSUPPLY.get_total_supply totalsupplies in
                let new_snapshots = update_totalsupply_snapshot current_scheduled_snapshot 0n total_supply_balance new_snapshots in
                new_snapshots
            in
            new_snapshots
        in
        new_snapshots



let update (p : TZIP12.transfer) (ledger: FA2.SingleAssetExtendable.ledger) (totalsupplies: TOTALSUPPLY.t) (snapshots:t) : t =
    let process_atomic_transfer (from_:address) (acc_snapshots, tr: t * TZIP12.atomic_trans) =
        let {to_;token_id;amount=amount_} = tr in
        update_atomic (Some(from_), Some(to_), amount_, token_id) ledger totalsupplies acc_snapshots
    in
    let process_single_transfer (acc_snapshots, tr: t * TZIP12.transfer_from ) =
        let {from_;txs} = tr in
        List.fold_left (process_atomic_transfer from_) acc_snapshots txs 
    in
    List.fold_left process_single_transfer snapshots p