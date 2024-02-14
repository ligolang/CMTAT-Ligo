type role = MINTER | BURNER | PAUSER | RULER | VALIDATOR | SNAPSHOOTER

type token_id = nat
type t = {
    general : ((address * role), unit) big_map;
    specific : ((address * token_id * role), unit) big_map;
}

module Errors = struct
    let unknown_user = "CMTAT_ROLE_UNKNOWN_USER" //"Unknown user (no role)"
    let missing_role = "CMTAT_ROLE_MISSING" //"User do not have this role"
    let not_ruler = "CMTAT_ROLE_NOT_RULER" //"This user is not allowed to modify rules"
    let not_minter = "CMTAT_ROLE_NOT_MINTER" //"This user is not allowed to mint assets"
    let not_burner = "CMTAT_ROLE_NOT_BURNER" //"This user is not allowed to burn assets"
    let not_pauser = "CMTAT_ROLE_NOT_PAUSER" //"This user is not allowed to pause the contract"
    let not_validator = "CMTAT_ROLE_NOT_VALIDATOR" //"This user is not allowed to change rule engine contract address"
    let not_snapshooter = "CMTAT_ROLE_NOT_SNAPSHOOTER" //"This user is not allowed to schedule snapshots"
    let role_already_granted = "CMTAT_ROLE_ALREADY_GRANTED" //"This user has already been granted this role for this token_id"
end

type grant_role_param = address * token_id option * role
let grantRole (p: grant_role_param) (roles: t) : t =
    match p.1 with
    | None -> (
        match Big_map.find_opt (p.0, p.2) roles.general with
        | None -> { roles with general = Big_map.update (p.0, p.2) (Some(())) roles.general }
        | Some(_) -> failwith Errors.role_already_granted
    )
    | Some(token_id) -> (
        match Big_map.find_opt (p.0, token_id, p.2) roles.specific with
        | None -> { roles with specific = Big_map.update (p.0, token_id, p.2) (Some(())) roles.specific }
        | Some(_) -> failwith Errors.role_already_granted
    )

type revoke_role_param = address * token_id option * role
let revokeRole (p: revoke_role_param) (roles: t) : t =
    match p.1 with
    | None -> (
        match Big_map.find_opt (p.0, p.2) roles.general with
        | None -> failwith Errors.missing_role
        | Some(_) -> { roles with general = Big_map.remove (p.0, p.2) roles.general }
    )
    | Some(token_id) -> (
        match Big_map.find_opt (p.0, token_id, p.2) roles.specific with
        | None -> failwith Errors.missing_role
        | Some(_) -> { roles with specific = Big_map.remove (p.0, token_id, p.2) roles.specific }
    )

type has_role_param = address * token_id option * role
let hasRole (p: has_role_param) (roles: t) : bool =
    match p.1 with
    | None -> (
        match Big_map.find_opt (p.0, p.2) roles.general with
        | None -> false
        | Some(_) -> true
    )
    | Some(token_id) -> (
        match Big_map.find_opt (p.0, token_id, p.2) roles.specific with
        | None -> false
        | Some(_) -> true
    )