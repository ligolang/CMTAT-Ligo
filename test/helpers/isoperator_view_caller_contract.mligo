#import "../../lib/main.mligo" "CMTAT"

type storage = bool option
type ret = operation list * storage

type operator = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE.FA2.SingleAssetExtendable.TZIP12.operator

[@entry]
let request (fa2_addr, op : address * operator) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "is_operator" op fa2_addr with
    | None -> failwith "is_operator not found"
    | Some(v) -> v
    in
    [] , (Some(new_storage))
