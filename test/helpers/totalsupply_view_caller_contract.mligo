type storage = nat
type ret = operation list * storage

[@entry]
let request (fa2_addr, tokenId : address * nat) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "total_supply" tokenId fa2_addr with
    | None -> failwith "Total supply not found"
    | Some(v) -> v
    in
    [] , new_storage
