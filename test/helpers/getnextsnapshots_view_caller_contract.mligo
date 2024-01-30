type storage = timestamp list
type ret = operation list * storage

[@entry]
let request (fa2_addr : address) (_store : storage) : ret = 
    let new_storage = match Tezos.call_view "getNextSnapshots" () fa2_addr with
    | None -> failwith "GetNextSnapshots view not found"
    | Some(v) -> v
    in
    [] , new_storage
