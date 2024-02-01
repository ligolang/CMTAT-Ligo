
type t = {
    admin : address;
    paused : bool;
    killed : bool;
}

module Errors = struct
    let not_admin = "Not admin"
    let contract_in_pause = "The contract is paused"
    let contract_killed = "The contract is killed"
end

type pause_param = bool
let pause (p: pause_param) (state: t) : t =
    // let () = assert_with_error (Tezos.get_sender() = s.extension.admin) Errors.not_admin in
    { state with paused = p }

let assert_not_paused (state: t) : unit = 
    assert_with_error (state.paused = false) Errors.contract_in_pause

let kill (state: t) : t =
    // let () = assert_with_error (Tezos.get_sender() = s.extension.admin) Errors.not_admin in
    { state with killed = true }

let assert_not_killed (state: t) : unit = 
    assert_with_error (state.killed = false) Errors.contract_killed