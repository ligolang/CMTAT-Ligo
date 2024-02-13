type t = {
    admin : address;
    paused : bool;
    killed : bool;
}

module Errors = struct
    let not_admin = "CMTAT_NOT_ADMIN" //"Not admin"
    let contract_in_pause = "CMTAT_CONTRACT_PAUSED" //"The contract is paused"
    let contract_killed = "CMTAT_CONTRACT_KILLED" //"The contract is killed"
end

type pause_param = bool
let pause (p: pause_param) (state: t) : t =
    { state with paused = p }

let assert_not_paused (state: t) : unit = 
    assert_with_error (state.paused = false) Errors.contract_in_pause

let kill (state: t) : t =
    { state with killed = true }

let assert_not_killed (state: t) : unit = 
    assert_with_error (state.killed = false) Errors.contract_killed