type role = MINTER | BURNER | PAUSER | RULER | VALIDATOR | SNAPSHOOTER

type token_id = nat
type t = ((address * role), unit) big_map  //(address, role set) big_map

module Errors = struct
    let unknown_user = "CMTAT_ROLE_UNKNOWN_USER" //"Unknown user (no role)"
    let missing_role = "CMTAT_ROLE_MISSING" //"User do not have this role"
    let not_ruler = "CMTAT_ROLE_NOT_RULER" //"This user is not allowed to modify rules"
    let not_minter = "CMTAT_ROLE_NOT_MINTER" //"This user is not allowed to mint assets"
    let not_burner = "CMTAT_ROLE_NOT_BURNER" //"This user is not allowed to burn assets"
    let not_pauser = "CMTAT_ROLE_NOT_PAUSER" //"This user is not allowed to pause the contract"
    let not_validator = "CMTAT_ROLE_NOT_VALIDATOR" //"This user is not allowed to change rule engine contract address"
    let not_snapshooter = "CMTAT_ROLE_NOT_SNAPSHOOTER" //"This user is not allowed to schedule snapshots"
    let role_already_granted = "CMTAT_ROLE_ALREADY_GRANTED" //"This user has already been granted this role"
end

type grant_role_param = address * role
let grantRole (p: grant_role_param) (roles: t) : t =
    match Big_map.find_opt p roles with
    | None -> Big_map.update p (Some(())) roles
    | Some(_) -> failwith Errors.role_already_granted

type revoke_role_param = address * role
let revokeRole (p: revoke_role_param) (roles: t) : t =
    match Big_map.find_opt p roles with
    | None -> failwith Errors.missing_role
    | Some(_) -> Big_map.remove p roles

type has_role_param = address * role
let hasRole (p: has_role_param) (roles: t) : bool =
    match Big_map.find_opt p roles with
    | None -> false
    | Some(_) -> true