# UNDER CONSTRUCTION !! 

TODO list
- interface jsligo


# CMTA Token

This repository is an implementation in cameligo of a CMTA token described by the [specifications](https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf).

It includes features from the basic token standard (TZIP12) and extra features for managing the total supply, for providing snapshots, and granting authorization to other users.

This implementation is architectured as a library which can be used to create a CMTA token. 
This library is extendable which means that when creating a CMTA token, it is possible to override features and to extend the storage with extra fields. These extra features can rely on extra fields in the storage and thus allow to create custom token with CMTA features and additionnal custom features.

This library relies on the `@ligo/fa` library which implements the TZIP-12 standard features and provides an implementation for fungible single-asset, fungible multi-asset , and non-fungible token. In the same manner the CMTA Token library also supports these 3 kind of tokens.

## How to use this library

Install the library with ligo CLI (with docker)
```
ligo install cmtat
```
This command will add a dependency in a `ligo.json` file. 

Here is an example of the resulting `ligo.json` file.
```
{ "dependencies": { "cmtat": "^1.0.0" } }
```

Once installed, you can use the cmtat library to create a cmtat token, or design a custom cmtat token by using the modules provided by the library.

## Configurations (asset type and extendable)

The library provides a default CMTAT Token implementation which declares all entrypoints. This default CMTAT Token implementation is not extendable (i.e. the storage cannot be extended with extra fields).

The following asset types are supported
- Single asset
- multi asset
- NFT


The library also provides an **extendable** implementation which helps to create a custom CMTAT Token implementation. Notice that the custom CMTAT Token must declare its entrypoints and can rely on the **extendable** implementation modules.


The following modules are provided by the library

| type  | extendable | module name    |
|-------|------------|----------------|
| single|    No      | CMTAT_SINGLE_ASSET                |
| single|    Yes     | CMTAT_SINGLE_ASSET_EXTENDABLE     |
| multi |    No      | CMTAT_MULTI_ASSET                 |
| multi |    Yes     | CMTAT_MULTI_ASSET_EXTENDABLE      |
| nft   |    No      | CMTAT_NFT_ASSET                   |
| nft   |    Yes     | CMTAT_NFT_ASSET_EXTENDABLE        |

## How to implement a custom CMTA Token

The library provides an **extendable** implementation which consist on a module that can be aliased for readingness.
```
#import "@cmtat/lib/main.mligo" "CMTAT"
module Token = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE
``` 
Now we can use all sub modules to complete the implementation of the custom CMTA Token.

### Sub-modules

The CMTAT library provides features separated on sub-modules which can be used in the final token contract. This *Token contract* must declare its entrypoints and can use functions implemented in the CMTAT library.

For example, the declaration of the `mint` entrypoint can rely on the default implementation provided by the module. 

The `storage` definition can rely on the one provided by the module. It is an extendable storage which expects a type. (Use `unit` if no extension)

```
#import "@cmtat/lib/main.mligo" "CMTAT"

module Token = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE
type storage = unit Token.storage
type ret = unit Token.ret

[@entry]
let mint (p: Token.mint_param) (s: storage) : ret =
  Token.mint p s
```
In the snippet of code (above) the `Token.mint` function is the default implementation. Notice that this token is a "Single asset" because the `CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE` module is used. This module also provides an interface (`Token.mint_param`) and a default storage (`Token.storage`).  

### Extendable library

By "extendable" it is meant that the storage can be extended. This module has been designed to allow to add extra field in the storage. The `CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE` module provides a parametric storage which expects an `extension` custom type.

For example one can define an empty extension by providing a `unit` type
```
type storage = unit Token.storage
```

or add new fields in an "extension" type
```
type extension = {
  issuer : address
}
type storage = extension Token.storage
type ret = extension Token.ret
```

This extendability is essential to allow to anyone to create tokens which inherit from CMTA Token behavior. 

One may want to override the default behavior provided by the `CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE` module.
Here's an example of an override where the pause entrypoint can only be called by the `issuer` (that we just defined previously in the extension) or the administrator (thus ignoring the PAUSER role).

```
[@entry]
let pause (p : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
  let sender = Tezos.get_sender() in
  let () = assert_with_error ((sender = s.extension.issuer) || (sender = s.administration.admin)) Errors.not_issuer_nor_admin in
  [], { s with administration = Token.ADMINISTRATION.pause p s.administration }
```
Notice that in this snippet of code, we do not use the `Token.pause` function but we use the sub-module `Token.ADMINISTRATION` in order to squizz the default role verification.

The error message is defined in an `Errors` sub module.
```
module Errors = struct
  let not_issuer_nor_admin = "Not issuer not admin"
end
```

### Declare entrypoints 

In order to finish the implementation of the custom CMTA Token, all expected entrypoints must be declared (and implemented).

Check the test [example](./test/cmtat/extended_cmtat_single_asset.instance.mligo) for an exhaustive snippet of code that defines all entrypoints. (Careful ! in the example the `pause` entrypoint has been overridden, consider the commented section instead).

```
[@entry]
let transfer (t : Token.TZIP12.transfer) (s : storage) : ret =
  Token.transfer t s

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  Token.get_balance p s


// [@entry]
// let pause (t : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
//   Token.pause t s

// Exemple of override of pause Entrypoint
[@entry]
let pause (p : Token.ADMINISTRATION.pause_param) (s : storage) : ret =
  let sender = Tezos.get_sender() in
  let () = assert_with_error ((sender = s.extension.issuer) || (sender = s.administration.admin)) Errors.not_issuer_nor_admin in
  [], { s with administration = Token.ADMINISTRATION.pause p s.administration }

[@entry]
let mint (p: Token.mint_param) (s: storage) : ret =
  Token.mint p s

[@entry]
let burn (p: Token.burn_param) (s: storage) : ret =
  Token.burn p s
```


## CMTAT sub modules

### FA2 (TZIP-12 standard)

FA2 basic features (from standard) consists on allowing people to transfer assets between them  

### Total supply

The *Totalsupply* module consists on keep track of the total supply of the token. The total supply represents the total number of assets. It is updated when some assets are minted or burned. 

### Administration

The *Administration* module consists on allowing a special user (administrator) to pause/unpause the contract, and to create assets and to destroy assets. 

The *Administration* module also provide a `kill` function to destroy the contract. Actually the contract still exist but the storage is cleaned and the entrypoints are disabled.


### Authorizations

The *Authorizations* module allows the `administrator` of the token to delegate some of his responsabilities to other trusted persons. It allows to grant administration roles to other trusted user. 

Being granted of a role allows to call specific entrypoints. Here is a list of roles and corresponding entrypoints.

|        Role       |    Entrypoints    |
|-------------------|-------------------|
| PAUSER            | pause |
| MINTER            | mint |
| BURNER            | burn |
| RULER             | grantRole, revokeRole |
| SNAPSHOOTER       | scheduleSnapshot, rescheduleSnapshot, unscheduleSnapshot |
| VALIDATOR         | setRuleEngine |

When calling one of these entrypoints a role verification is performed. The diagram illustrates this verification for the pause entrypoint. 

```mermaid
stateDiagram-v2
    state is_admin_state <<choice>>
    state is_pauser_state <<choice>>
    change_paused_flag: Change paused flag
    [*] --> is_admin_state
    is_admin_state --> change_paused_flag: if sender = admin
    is_admin_state --> is_pauser_state : if sender <> admin
    is_pauser_state --> change_paused_flag: if sender has PAUSER role
    is_pauser_state --> NOT_PAUSER: if sender has not PAUSER role
    change_paused_flag --> [*] 
```
The role verification is similar for all entrypoints. 

The *Authorizations* module provides two functions (`grantRole`, `revokeRole`) to modify roles for a given user.

The *Authorizations* module also provides a function (`hasRole`) to verify if a given user has a given role .

Attention ! Specifically for Multi asset configuration, an additonnal mecanism for role specific per `token_id` has been added. Though it is not applied to all roles ! Some roles must not be related to a single `token_id`. 
- MINTER, BURNER, RULER roles can be specific to a `token_id` 
- PAUSER, SNAPSHOOTER, VALIDATOR roles are global (cannot not be related to a specific `token_id`). In other words, the fonctions (pause, scheduleSnapshot, rescheduleSnapshot, unscheduleSnapshot, SetRuleEngine) expects a global role. For example, a user granted with a SNAPSHOOTER role on token_id 1 will not be able to schedule a snapshot (even on token_id 1) ! 


### Validation

The *Validation* module provides an external mecanism to authorize/unauthorize transfer of asset. 

It is required to be able to prevent the execution of a transfer depending on arbitrary rules (such as specific account can be blacklisted). These rules are externalized in an other contract (called **RuleEngine**). 

The *Validation* module provides a function `validateTransfer` that asks the **RuleEngine** contract if a transfer is allowed. This function is called during a `Transfer` (in the [implementation](./lib/cmtat/extendable_cmtat_single_asset.impl.mligo) of `transfer` function in the CMTAT library). 

The *Validation* module provides a function `setRuleEngine` that modifies the **RuleEngine** contract that is used to unauthorize a transfer. This function expects the `address` of the new **RuleEngine** contract. This function is only callable by the administrator of the token.

A example of **RuleEngine** contract is provided in the `test` directory. This [example contract](./test/helpers/rule_engine_contract.mligo) implements a naive blacklist and verifies that a given transaction does not involve blcklisted addresses.
A RuleEngine contract must have an on-chain view with the following signature:
```
let validateTransfer (from_, to_, _amount_ : address * address * nat) (s : storage) : bool =
```

In the case a **RuleEngine** has been defined, the validation workflow of a `transfer` involves a RuleEngine contract.
```mermaid
sequenceDiagram
    actor Alice
    Alice->>+CMTAT: transfer 10 to John
    CMTAT->>RULE_ENGINE: call view validateTransfer
    RULE_ENGINE-->>CMTAT: authorized ? unauthorized
    CMTAT->>CMTAT: update snapshots
    CMTAT->>-CMTAT: proceed transfer if authorized
```

### Snapshots

The *Snapshots* module keeps track of total supply and account balance at certain point in time.


The *Snapshots* module provides the `scheduleSnapshot` function which allows to schedule snapshots (in the future).  
When the Transfer entrypoint (or Mint Burn) is called it updates the total supply and account balances inside the current snapshot. 
TODO  ==== This update is done before the execution of the transfer, thus snapshots represents the balance before the transfer is done.

The *Snapshots* module provides the `rescheduleSnapshot` function to modify when a snapshot is scheduled (the scheduled snapshots cannot be re-ordered).

The *Snapshots* module provides the `unscheduleSnapshot` function to cancel the last scheduled snapshot.

The *Snapshots* module provides a view `getNextSnapshots` to retrieve the existing scheduled snapshot times (ones not yet done). So the first one is the current snapshot.

The *Snapshots* module also provides views (`snapshotTotalsupply`, `snapshotBalanceOf`)to query the total supply and account balances for a given snapshot time.



## Library functions

Functions provided by the library for Single asset configuration.

| function name           | parameter                                    |  module                   |
|-------------------------|----------------------------------------------|---------------------------|
| pause                   | bool                                         | ADMINISTRATION            |
| transfer                | FA2.SingleAssetExtendable.TZIP12.transfer    | FA2.SingleAssetExtendable |
| balance_of              | FA2.SingleAssetExtendable.TZIP12.balance_of  | FA2.SingleAssetExtendable |
| update_operators        | FA2.SingleAssetExtendable.TZIP12.update_operators | FA2.SingleAssetExtendable |
| mint                    | mint_param                                   | FA2.SingleAssetExtendable |
| burn                    | burn_param                                   | FA2.SingleAssetExtendable |
| kill                    | unit                                         | ADMINISTRATION            |
| grantRole               | address * AUTHORIZATIONS.role                | AUTHORIZATIONS            |
| revokeRole              | address * AUTHORIZATIONS.role                | AUTHORIZATIONS            |
| scheduleSnapshot        | timestamp                                    | SNAPSHOTS                 |
| rescheduleSnapshot      | timestamp * timestamp                        | SNAPSHOTS                 |
| unscheduleSnapshot      | timestamp                                    | SNAPSHOTS                 |
| setRuleEngine           | address option                               | VALIDATION                |
| validateTransfer        | address * address * nat                      | VALIDATION                |
| assert_validateTransfer | TZIP12.transfer                              | VALIDATION                |

Functions provided by the library for Multi asset configuration.

| function name           | parameter                                    |  module                   |
|-------------------------|----------------------------------------------|---------------------------|
| pause                   | bool                                         | ADMINISTRATION            |
| transfer                | TZIP12.transfer                              | FA2.MultiAssetExtendable  |
| balance_of              | TZIP12.balance_of                            | FA2.MultiAssetExtendable  |
| update_operators        | TZIP12.update_operators                      | FA2.MultiAssetExtendable  |
| mint                    | mint_param                                   | FA2.MultiAssetExtendable  |
| burn                    | burn_param                                   | FA2.MultiAssetExtendable  |
| kill                    | unit                                         | ADMINISTRATION            |
| grantRole               | address * nat option * AUTHORIZATIONS.role   | AUTHORIZATIONS            |
| revokeRole              | address * nat option * AUTHORIZATIONS.role   | AUTHORIZATIONS            |
| scheduleSnapshot        | timestamp                                    | SNAPSHOTS                 |
| rescheduleSnapshot      | timestamp * timestamp                        | SNAPSHOTS                 |
| unscheduleSnapshot      | timestamp                                    | SNAPSHOTS                 |
| setRuleEngine           | address option                               | VALIDATION                |
| validateTransfer        | address * address * nat                      | VALIDATION                |
| assert_validateTransfer | TZIP12.transfer                              | VALIDATION                |


## Library Views

Views provided by the library for Single asset configuration.

| view name           | parameter                                 | returned type  | module                    |
|---------------------|-------------------------------------------|----------------|---------------------------|
| get_balance         | address * nat                             | nat            | FA2.SingleAssetExtendable |
| total_supply        | nat                                       | nat            | FA2.SingleAssetExtendable |
| all_tokens          | unit                                      | nat set        | FA2.SingleAssetExtendable |
| is_operator         | FA2.SingleAssetExtendable.TZIP12.operator | bool           | FA2.SingleAssetExtendable |
| token_metadata      | nat                                       | FA2.SingleAssetExtendable.TZIP12.tokenMetadataData  | FA2.SingleAssetExtendable |
| hasRole             | address * AUTHORIZATIONS.role             | bool           | AUTHORIZATIONS            |
| getNextSnapshots    | unit                                      | timestamp list | SNAPSHOTS                 |
| snapshotTotalsupply | timestamp * nat                           | nat            | SNAPSHOTS                 |
| snapshotBalanceOf   | timestamp * address * nat                 | nat            | SNAPSHOTS                 |

## Error messages

| module         |  const                 | message code                | Explanation                                                              | triggered by   |
|----------------|------------------------|-----------------------------|--------------------------------------------------------------------------|----------------|
| ADMINISTRATION | not_admin              | CMTAT_NOT_ADMIN             | Not admin                                                                | kill |
| ADMINISTRATION | contract_in_pause      | CMTAT_CONTRACT_PAUSED       | The contract is paused                                                   | \<all\> |
| ADMINISTRATION | contract_killed        | CMTAT_CONTRACT_KILLED       | The contract is killed                                                   | \<all\> |
| TOTALSUPPLY    | not_enough_supply      | CMTAT_TOTALSUPPLY_INSUFFICIENT_BALANCE  | Not enough supply                                            | burn |
| SNAPSHOTS      | schedule_in_past       | CMTAT_SCHEDULE_IN_PAST      | Cannot schedule in the past                                              | scheduleSnapshot, rescheduleSnapshot |
| SNAPSHOTS      | before_next_scheduled  | CMTAT_SCHEDULE_BEFORE_NEXT  | Proposed scheduled is before the next scheduled time                     | scheduleSnapshot |
| SNAPSHOTS      | already_scheduled      | CMTAT_SCHEDULE_ALREADY_SCHEDULED  | This snapshot time is already scheduled                            | scheduleSnapshot |
| SNAPSHOTS      | rescheduled_after_next | CMTAT_RESCHEDULE_AFTER_NEXT | New scheduled is after next scheduled                                    | rescheduleSnapshot |
| SNAPSHOTS      | rescheduled_before_previous | CMTAT_RESCHEDULE_BEFORE_PREVIOUS  | New scheduled is before previous scheduled                    | rescheduleSnapshot |
| SNAPSHOTS      | snapshot_already_done  | CMTAT_SNAPSHOT_ALREADY_DONE | Snapshot already done                                                    | unscheduleSnapshot, rescheduleSnapshot |
| SNAPSHOTS      | no_snapshot_scheduled  | CMTAT_NO_SCHEDULED_SNAPSHOT | No scheduled snapshot                                                    | unscheduleSnapshot |
| SNAPSHOTS      | snapshot_not_found     | CMTAT_SNAPSHOT_UNKNOWN      | Snapshot not found                                                       | unscheduleSnapshot |
| VALIDATION     | undefined_rule_engine  | CMTAT_RULE_ENGINE_UNDEFINED | Rule engine not defined                                                  | ?  |
| VALIDATION     | refused_by_rule_engine | CMTAT_RULE_ENGINE_REFUSED   | NOT_VALIDATED by rule engine                                             | assert_validateTransfer |
| VALIDATION     | invalid_rule_engine    | CMTAT_RULE_ENGINE_INVALID   | The pointed rule engine does not have an on-chain view validateTransfer  | validateTransfer |
| AUTHORIZATIONS | unknown_user           | CMTAT_ROLE_UNKNOWN_USER     | Unknown user (no role)                                                   | revokeRole |
| AUTHORIZATIONS | missing_role           | CMTAT_ROLE_MISSING          | User do not have this role                                               | revokeRole |
| AUTHORIZATIONS | not_ruler              | CMTAT_ROLE_NOT_RULER        | This user is not allowed to modify rules                                 | grantRole, revokeRole |
| AUTHORIZATIONS | not_minter             | CMTAT_ROLE_NOT_MINTER       | This user is not allowed to mint assets                                  | mint |
| AUTHORIZATIONS | not_burner             | CMTAT_ROLE_NOT_BURNER       | This user is not allowed to burn assets                                  | burn |
| AUTHORIZATIONS | not_pauser             | CMTAT_ROLE_NOT_PAUSER       | This user is not allowed to pause the contract                           | pause |
| AUTHORIZATIONS | not_validator          | CMTAT_ROLE_NOT_VALIDATOR    | This user is not allowed to change rule engine contract address          | setRuleEngine |
| AUTHORIZATIONS | not_snapshooter        | CMTAT_ROLE_NOT_SNAPSHOOTER  | This user is not allowed to schedule snapshots                           | scheduleSnapshot, rescheduleSnapshot,unscheduleSnapshot   |





## Tests

An exemple of CMTA Token contract has been implemented for testing purposes. This [extended_cmtat_single_asset](./test/cmtat/extended_cmtat_single_asset.instance.mligo) illustrates 
- how to implement a token contract based on cmtat library 
- how to define entrypoints using the default behavior
- how to customize the storage of the token contract
- how to customize entrypoints while reusing sub-modules of the library

This token contract is used to originate a contract instance and to run test on related entrypoints. [Tests](./test/cmtat/extended_cmtat_single_asset.test.mligo) of the **extended_cmtat_single_asset** contract illusrates how to execute entrypoints in predefined conditions and how to verify execution termination (success and failures).

These tests also ensures that the default implementation provided by the library is doing what is expected of it !

To run these tests, run the following commands
```
git clone https://github.com/ligolang/CMTAT-Ligo.git
cd CMTAT-Ligo
make install
make test
```

To run test only one file. The SUITE env var can be set to point the test file. 
For example, for the basic FA2 tests for a multi asset `SUITE=cmtat/SUITE=cmtat/extended_cmtat_multi_asset.fa2` points to `./test/cmtat/SUITE=cmtat/extended_cmtat_multi_asset.fa2.test.mligo`
```
make test SUITE=cmtat/extended_cmtat_multi_asset.fa2
```

To run tests on CMTAT features for a multi asset
```
make test SUITE=cmtat/extended_cmtat_multi_asset
```

To run tests on CMTAT features for a single asset
```
make test SUITE=cmtat/extended_cmtat_single_asset
```

To run tests on FA2 features for a single asset
```
make test SUITE=cmtat/extended_cmtat_single_asset.fa2
```

To run tests on CMTAT features for a NFT
```
make test SUITE=cmtat/extended_cmtat_nft_asset
```