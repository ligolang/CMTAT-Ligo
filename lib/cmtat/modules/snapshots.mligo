// #import "../lib/multi_asset/fa2.mligo" "FA2"
#import "@ligo/fa/lib/main.mligo" "FA2"

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


let update_atomic (tr: address * address * nat * nat) (snapshots: t) : t =
    let (from_, to_, amt, _token_id) = tr in
    let () = assert_with_error (amt > 0n) "Transfer ZERO" in
    snapshots


let update (p : TZIP12.transfer) (snapshots:t) : t =
    let process_atomic_transfer (from_:address) (acc_snapshots, tr: t * TZIP12.atomic_trans) =
        let {to_;token_id;amount=amount_} = tr in
        update_atomic (from_, to_, amount_, token_id) acc_snapshots
    in
    let process_single_transfer (acc_snapshots, tr: t * TZIP12.transfer_from ) =
        let {from_;txs} = tr in
        List.fold_left (process_atomic_transfer from_) acc_snapshots txs 
    in
    List.fold_left process_single_transfer snapshots p