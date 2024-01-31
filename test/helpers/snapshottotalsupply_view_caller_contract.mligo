type storage = nat
type ret = operation list * storage

[@entry]
let request (fa2_addr, time, tokenId : address * timestamp * nat) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "snapshotTotalsupply" (time, tokenId) fa2_addr with
    | None -> failwith "SnapshotTotalsupply not found"
    | Some(v) -> v
    in
    [] , new_storage
