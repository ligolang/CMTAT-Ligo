type amount_ = nat
type t = amount_

module Errors = struct
    let not_enough_supply = "CMTAT_TOTALSUPPLY_INSUFFICIENT_BALANCE" //"Not enough supply"
end

let get_total_supply (totalsupplies:t) : amount_ =
    totalsupplies

let set_total_supply (_totalsupplies:t) (amount_:amount_) : t =
    amount_

let increase_token_total_supply (totalsupplies:t)  (mint_value: nat) = 
    let balance = get_total_supply totalsupplies in
    let new_balance = balance + mint_value in
    set_total_supply totalsupplies new_balance

let decrease_token_total_supply (totalsupplies:t) (burn_value: nat) = 
    let balance = get_total_supply totalsupplies in
    let () = assert_with_error (balance >= burn_value) Errors.not_enough_supply in
    let new_balance = abs(balance - burn_value) in
    set_total_supply totalsupplies new_balance
