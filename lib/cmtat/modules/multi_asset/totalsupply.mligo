

//TODO
type token_id = nat
type amount_ = nat
type t = (token_id, amount_) big_map

module Errors = struct
    let not_enough_supply = "Not enough supply"
end

let get_total_supply (totalsupplies:t) (token_id : token_id) : amount_ =
    match Big_map.find_opt token_id totalsupplies with Some (a) -> a | None -> 0n

let set_total_supply (totalsupplies:t) (token_id : token_id ) (amount_:amount_) : t =
    Big_map.update token_id (Some amount_) totalsupplies

let increase_token_total_supply (totalsupplies:t) (tok: token_id) (mint_value: nat) = 
    let balance = get_total_supply totalsupplies tok in
    let new_balance = balance + mint_value in
    set_total_supply totalsupplies tok new_balance

let decrease_token_total_supply (totalsupplies:t) (tok: token_id) (burn_value: nat) = 
    let balance = get_total_supply totalsupplies tok in
    let () = assert_with_error (balance >= burn_value) Errors.not_enough_supply in
    let new_balance = abs(balance - burn_value) in
    set_total_supply totalsupplies tok new_balance
