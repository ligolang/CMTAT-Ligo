type role = MINTER | BURNER | PAUSER | RULER | VALIDATOR | SNAPSHOOTER

type t = (address, role set) big_map

module Errors = struct
    let unknown_user = "CMTAT_ROLE_UNKNOWN_USER" //"Unknown user (no role)"
    let missing_role = "CMTAT_ROLE_MISSING" //"User do not have this role"
    let not_ruler = "CMTAT_ROLE_NOT_RULER" //"This user is not allowed to modify rules"
    let not_minter = "CMTAT_ROLE_NOT_MINTER" //"This user is not allowed to mint assets"
    let not_burner = "CMTAT_ROLE_NOT_BURNER" //"This user is not allowed to burn assets"
    let not_pauser = "CMTAT_ROLE_NOT_PAUSER" //"This user is not allowed to pause the contract"
    let not_validator = "CMTAT_ROLE_NOT_VALIDATOR" //"This user is not allowed to change rule engine contract address"
    let not_snapshooter = "CMTAT_ROLE_NOT_SNAPSHOOTER" //"This user is not allowed to schedule snapshots"
end

type grant_role_param = address * role
let grantRole (p: grant_role_param) (roles: t) : t =
    match Big_map.find_opt p.0 roles with
    | None -> Big_map.update p.0 (Some(Set.add p.1 Set.empty)) roles
    | Some(flags) -> Big_map.update p.0 (Some(Set.add p.1 flags)) roles


type revoke_role_param = address * role
let revokeRole (p: revoke_role_param) (roles: t) : t =
    match Big_map.find_opt p.0 roles with
    | None -> failwith Errors.unknown_user
    | Some(flags) -> 
        if Set.mem p.1 flags then
            Big_map.update p.0 (Some(Set.remove p.1 flags)) roles
        else
            failwith Errors.missing_role

type has_role_param = address * role
let hasRole (p: has_role_param) (roles: t) : bool =
    match Big_map.find_opt p.0 roles with
    | None -> false
    | Some(flags) -> Set.mem p.1 flags
