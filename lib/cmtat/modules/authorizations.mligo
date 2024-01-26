type role = MINTER | BURNER | PAUSER | RULER | FREEZER | SNAPSHO0TER

type t = (address, role set) big_map

module Errors = struct
    let unknown_user = "Unknown user (no role)"
    let missing_role = "User do not have this role"

    let not_ruler = "This user is not allowed to modify rules"
    let not_minter = "This user is not allowed to mint assets"
    let not_burner = "This user is not allowed to burn assets"
    let not_pauser = "This user is not allowed to pause the contract"
    let not_freezer = "This user is not allowed to freeze holders"
    let not_snapshooter = "This user is not allowed to schedule snapshots"
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
