type storage = nat
type ret = operation list * storage

[@entry]
let request (fa2_addr, time, user, tokenId : address * timestamp * address * nat) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "snapshotBalanceOf" (time, user, tokenId) fa2_addr with
    | None -> failwith "SnapshotBalanceOf not found"
    | Some(v) -> v
    in
    [] , new_storage
