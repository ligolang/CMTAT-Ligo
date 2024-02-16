#import "@ligo/fa/lib/main.mligo" "FA2"
#import "./totalsupply.mligo" "TOTALSUPPLY"

module TZIP12 = FA2.MultiAssetExtendable.TZIP12


type snapshot_data = nat

type snapshots = ((timestamp * nat), snapshot_data) big_map

type t = {
    account_snapshots : ((address * timestamp * nat), snapshot_data) big_map;
    totalsupply_snapshots : snapshots;
    scheduled_snapshots : timestamp list
}

module Errors = struct
    let schedule_in_past = "CMTAT_SCHEDULE_IN_PAST" //"Cannot schedule in the past"
    let before_next_scheduled = "CMTAT_SCHEDULE_BEFORE_NEXT" //"Proposed scheduled is before the next scheduled time"
    let already_scheduled = "CMTAT_SCHEDULE_ALREADY_SCHEDULED" //"This snapshot time is already scheduled"
    let rescheduled_after_next = "CMTAT_RESCHEDULE_AFTER_NEXT" //"New scheduled is after next scheduled"
    let rescheduled_before_previous = "CMTAT_RESCHEDULE_BEFORE_PREVIOUS" //"New scheduled is before previous scheduled"
    let snapshot_already_done = "CMTAT_SNAPSHOT_ALREADY_DONE" //"Snapshot already done"
    let no_snapshot_scheduled = "CMTAT_NO_SCHEDULED_SNAPSHOT" //"No scheduled snapshot"
    let snapshot_not_found = "CMTAT_SNAPSHOT_UNKNOWN" //"Snapshot not found"
end

///////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
let reverse (type a) (xs : a list) : a list =
    let f (ys,x : (a list * a)) : a list = x :: ys in
    List.fold_left f ([] : a list) xs

let get_for_user_curried (ledger, owner, token_id: FA2.MultiAssetExtendable.ledger * address * nat) = 
    FA2.MultiAssetExtendable.get_for_user ledger owner token_id

let get_total_supply_curried (ledger, token_id: TOTALSUPPLY.t * nat) = 
    TOTALSUPPLY.get_total_supply ledger token_id

///////////////////////////////////////////////////////////////////////////////////////////////////////////

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

let rescheduleSnapshot (old_time : timestamp) (new_time : timestamp) (snapshots:t) : t =
    // check if in the past
    let () = assert_with_error (old_time > Tezos.get_now()) Errors.schedule_in_past in
    let () = assert_with_error (new_time > Tezos.get_now()) Errors.schedule_in_past in
    
    let new_scheduled = match snapshots.scheduled_snapshots with
    | [] -> []
    | [x] ->
        if (x = old_time) then
            [new_time]
        else
            [x]
    | _ -> 
        let assert_below_upper_bound (time: timestamp) (upperbound: timestamp) : unit = 
            if (time > upperbound) then
                failwith Errors.rescheduled_before_previous
            else if  (time = upperbound) then
                failwith Errors.already_scheduled
            else
                ()
        in
        let assert_above_lower_bound (time: timestamp) (lowerbound: timestamp) : unit = 
            if (time < lowerbound) then
                failwith Errors.rescheduled_after_next
            else if  (time = lowerbound) then
                failwith Errors.already_scheduled
            else
                ()
        in
        // verify lower bound and upper bound for each element of the list (except last one)
        let replace (acc, elt: timestamp list * timestamp) =
            match acc with
                | [] -> elt :: []
                | current::tl -> 
                    if (current = old_time) then
                        let next = elt in
                        let _ = assert_below_upper_bound next new_time in
                        let () = match tl with
                            | [] -> ()
                            | previous::_ -> assert_above_lower_bound  previous new_time
                        in
                        elt :: new_time :: tl
                    else
                        elt :: current :: tl
        in
        let new_scheduled_reversed = List.fold replace snapshots.scheduled_snapshots ([] : timestamp list) in
        // case of last element of the list
        let new_scheduled_reversed = match new_scheduled_reversed with
            | cur::prev::tl ->
                if (cur = old_time) then
                    let _ = assert_below_upper_bound new_time prev  in
                    new_time::prev::tl
                else
                    cur::prev::tl
            | _ -> new_scheduled_reversed
        in
        reverse new_scheduled_reversed
    in
    { snapshots with scheduled_snapshots = new_scheduled }

let unscheduleSnapshot (time : timestamp) (snapshots:t) : t =
    // assert not in the past
    let () = assert_with_error (time > Tezos.get_now()) Errors.snapshot_already_done in
    // remove first element
    let new_scheduled_snapshots = match snapshots.scheduled_snapshots with
    | [] -> failwith Errors.no_snapshot_scheduled
    | hd::tl -> if (hd = time) then tl else failwith Errors.snapshot_not_found
    in 
    { snapshots with scheduled_snapshots = new_scheduled_snapshots }

let getNextSnapshots (snapshots:t) : timestamp list =
    let filter_past (acc, elt: timestamp list * timestamp) = if (elt > Tezos.get_now()) then elt :: acc else acc in
    List.fold filter_past snapshots.scheduled_snapshots ([] : timestamp list)

// If there is a scheduled snapshot for the given time returns totalsupply of the snapshot otherwise returns the current totalsupply
let snapshotTotalsupply (time : timestamp) (token_id: nat) (totalsupplies: TOTALSUPPLY.t)  (snapshots:t) : nat =
    match Big_map.find_opt (time, token_id) snapshots.totalsupply_snapshots with
    | None -> (match (Big_map.find_opt token_id totalsupplies) with
        | Some(actual) -> actual
        | None -> 0n)
    | Some(v) -> v

// (utils) parse scheduled snapshot list to get the closest snapshot of a given timestamp
let get_next_scheduled_snapshot (ref_time: timestamp) (snapshots:t) : timestamp option =
    let get_next (acc, elt: timestamp option * timestamp) = match acc with
    | Some(time) -> if (ref_time < elt) && (elt < time) then Some(elt) else acc 
    | None -> if (ref_time < elt) then Some(elt) else acc
    in
    List.fold get_next snapshots.scheduled_snapshots (None: timestamp option)

// If there is a scheduled snapshot for the given time returns balance of the snapshot otherwise returns the current user balance
let snapshotBalanceOf (time : timestamp) (user: address) (token_id: nat) (ledger: FA2.MultiAssetExtendable.ledger) (snapshots:t) : nat =
    match Big_map.find_opt (user, time, token_id) snapshots.account_snapshots with
    | Some(v) -> v
    | None -> // search closest scheduled snapshot 
        (match (get_next_scheduled_snapshot time snapshots) with
        | None -> get_for_user_curried(ledger, user, token_id)
        | Some(tt) -> (
            match (Big_map.find_opt (user, tt, token_id) snapshots.account_snapshots) with
            | None -> get_for_user_curried(ledger, user, token_id) // nothing registered yet so get current value
            | Some(v) -> v
            )
        )

////////////////////////////////////////////////////////////////////////////////////////
//                          UPDATE
////////////////////////////////////////////////////////////////////////////////////////
let update_account_snapshot (current_scheduled_snapshot: timestamp) (token_id: nat) (account: address) (account_balance: nat) (snapshots: t) : t = 
    let new_account_snapshots = match Big_map.find_opt (account, current_scheduled_snapshot, token_id) snapshots.account_snapshots with
    | Some(_v) -> 
        Big_map.update (account, current_scheduled_snapshot, token_id) (Some(account_balance)) snapshots.account_snapshots
    | None() -> 
        Big_map.add (account, current_scheduled_snapshot, token_id) account_balance snapshots.account_snapshots
    in
    { snapshots with account_snapshots = new_account_snapshots }

let update_totalsupply_snapshot (current_scheduled_snapshot: timestamp) (token_id: nat) (totalsupply_balance: nat) (snapshots: t) : t = 
    let new_totalsupply_snapshots = Big_map.update (current_scheduled_snapshot, token_id) (Some(totalsupply_balance)) snapshots.totalsupply_snapshots in
    { snapshots with totalsupply_snapshots = new_totalsupply_snapshots }


let update_atomic (tr: address option * address option * nat * nat) (ledger: FA2.MultiAssetExtendable.ledger) (totalsupplies: TOTALSUPPLY.t) (snapshots: t) : t =
    let (from_, to_, amt, token_id) = tr in
    // Do nothing when amount is 0 or no recipient to modify 
    if (amt = 0n) || (Option.is_none(from_) && Option.is_none(to_)) then
        snapshots
    else
        // Retrieve current scheduled time
        let current_scheduled_snapshot_opt = get_next_scheduled_snapshot (Tezos.get_now()) snapshots in
        let new_snapshots = snapshots in
        let new_snapshots = match current_scheduled_snapshot_opt with
        | None -> snapshots
        | Some(current_scheduled_snapshot) -> 
            let new_snapshots = match (from_, to_) with
            | None, None  -> (failwith "SNAPSHOT internal error": t) // snapshots
            | None, Some(to_) -> // MINT
                // Update to_ account
                let to_balance = get_for_user_curried(ledger, to_, token_id) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot token_id to_ to_balance new_snapshots in
                // Update total supply
                let total_supply_balance = get_total_supply_curried (totalsupplies, token_id) in
                let new_snapshots = update_totalsupply_snapshot current_scheduled_snapshot token_id total_supply_balance new_snapshots in
                new_snapshots
            | Some(from_), Some(to_) -> // TRANSFER
                // Update from_ account 
                let from_balance = get_for_user_curried(ledger, from_, token_id) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot token_id from_ from_balance new_snapshots in
                // Update to_ account
                let to_balance = get_for_user_curried(ledger, to_, token_id) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot token_id to_ to_balance new_snapshots in
                new_snapshots
            | Some(from_), None -> // BURN
                // Update from_ account 
                let from_balance = get_for_user_curried(ledger, from_, token_id) in
                let new_snapshots = update_account_snapshot current_scheduled_snapshot token_id from_ from_balance new_snapshots in
                // Update total supply
                let total_supply_balance = get_total_supply_curried (totalsupplies, token_id) in
                let new_snapshots = update_totalsupply_snapshot current_scheduled_snapshot token_id total_supply_balance new_snapshots in
                new_snapshots
            in
            new_snapshots
        in
        new_snapshots



let update (p : TZIP12.transfer) (ledger: FA2.MultiAssetExtendable.ledger) (totalsupplies: TOTALSUPPLY.t) (snapshots:t) : t =
    let process_atomic_transfer (from_:address) (acc_snapshots, tr: t * TZIP12.atomic_trans) =
        let {to_;token_id;amount=amount_} = tr in
        update_atomic (Some(from_), Some(to_), amount_, token_id) ledger totalsupplies acc_snapshots
    in
    let process_single_transfer (acc_snapshots, tr: t * TZIP12.transfer_from ) =
        let {from_;txs} = tr in
        List.fold_left (process_atomic_transfer from_) acc_snapshots txs 
    in
    List.fold_left process_single_transfer snapshots p