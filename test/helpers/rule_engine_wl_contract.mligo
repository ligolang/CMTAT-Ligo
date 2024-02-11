#import "../../lib/main.mligo" "CMTAT"

type storage = {
    admin: address;
    authorized: (address, bool) big_map
}
type ret = operation list * storage

type transfer = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE.FA2.SingleAssetExtendable.TZIP12.transfer

[@entry]
let freeze (user: address) (s : storage) : ret =
    let () = assert_with_error (Tezos.get_sender() = s.admin) "not_admin" in
    [], { s with authorized=Big_map.update user (Some(false)) s.authorized }

[@entry]
let unfreeze (user: address) (s : storage) : ret =
    let () = assert_with_error (Tezos.get_sender() = s.admin) "not_admin" in
    [], { s with authorized=Big_map.update user (Some(true)) s.authorized }

[@view]
let validateTransfer (from_, to_, _amount_ : address * address * nat) (s : storage) : bool =
    let from_frozen = match Big_map.find_opt from_ s.authorized with
    | None -> false
    | Some(v) -> v
    in
    let to_frozen = match Big_map.find_opt to_ s.authorized with
    | None -> false
    | Some(v) -> v
    in
    (from_frozen && to_frozen)