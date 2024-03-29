#import "@ligo/fa/lib/main.mligo" "FA2"

module TZIP12 = FA2.SingleAssetExtendable.TZIP12

type t = {
    rule_engine_contract : address option;
}

module Errors = struct
    let undefined_rule_engine = "CMTAT_RULE_ENGINE_UNDEFINED" //"Rule engine not defined"
    let refused_by_rule_engine = "CMTAT_RULE_ENGINE_REFUSED" //"NOT_VALIDATED by rule engine"
    let invalid_rule_engine = "CMTAT_RULE_ENGINE_INVALID" //"The pointed rule engine does not have an on-chain view validateTransfer"
end

type rule_engine_param = address option
let set_rule_engine (p: rule_engine_param) (state: t) : t =
    { state with rule_engine_contract = p }

let has_rule_engine (state: t) : bool = 
    Option.is_some state.rule_engine_contract

let validateTransfer (from_, to_, amount_: address * address * nat) (state: t) : bool =
    if has_rule_engine state then
        match Tezos.call_view "validateTransfer" (from_, to_, amount_) (Option.unopt state.rule_engine_contract) with
        | None -> failwith Errors.invalid_rule_engine
        | Some(v) -> v
    else
        true

let assert_validateTransfer (tr: TZIP12.transfer) (state: t) : unit =
    let process_atomic (from_: address) (elt: TZIP12.atomic_trans) =
        assert_with_error (validateTransfer (from_, elt.to_, elt.amount) state) Errors.refused_by_rule_engine
    in
    let process_tr_from (tr_from: TZIP12.transfer_from) =
        List.iter (process_atomic tr_from.from_) tr_from.txs
    in
    List.iter process_tr_from tr