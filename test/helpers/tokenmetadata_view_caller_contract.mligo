#import "../../lib/main.mligo" "CMTAT"
type tokenMetadataData = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE.FA2.SingleAssetExtendable.TZIP12.tokenMetadataData

type storage = tokenMetadataData option
type ret = operation list * storage

[@entry]
let request (fa2_addr, token_id : address * nat) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "token_metadata" token_id fa2_addr with
    | None -> failwith "token_metadata not found"
    | Some(v) -> v
    in
    [] , (Some(new_storage))
