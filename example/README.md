# Example 

This section is dedicated to demonstrate how to make a CMTA Token using the `cmtat` library.

## Source code of the example contract

The source code of the example contract is available [here](./extended_cmtat_single_asset.mligo)

This example contract can be compiled
```
make compile_example
```
It will produce a JSON file in the `compiled` directory [here](./compiled/example/extended_cmtat_single_asset.mligo.json)

## Deployment of the example contract

The example contract can be deployed on the Ghostnet network
```
make deploy
```
This command will deploy the example contract and display its contract address.

The resulting expected display is
```
Waiting for singleAssetContract KT1AemVzUZmFjTTLDf9D5RukmYKh6qADEtJn to be confirmed...
confirmed contract:  KT1AemVzUZmFjTTLDf9D5RukmYKh6qADEtJn
```

Once deployed, metadata of the token can be seen with Tzcomet
[here](https://tzcomet.io/#/explorer%3Fexplorer-input%3DKT1AemVzUZmFjTTLDf9D5RukmYKh6qADEtJn).

TZKT is a Tezos indexer. The contract can be seen [here](https://ghostnet.tzkt.io/KT1AemVzUZmFjTTLDf9D5RukmYKh6qADEtJn/operations/).

BCD is a Tezos indexer which also allows to interact with our [deployed contract](https://better-call.dev/ghostnet/KT1AemVzUZmFjTTLDf9D5RukmYKh6qADEtJn/operations).

## How to implement a custom CMTA Token

In this section we will give some explanations about the source code of the example contract.  

The library provides an **extendable** implementation which consist on a module that can be aliased for readingness.
```
#import "@cmtat/lib/main.mligo" "CMTAT"
module Token = CMTAT.CMTAT_SINGLE_ASSET_EXTENDABLE
``` 
Notice that we choose the extendable "Single asset" configuration.
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

Notice that in the example contract the `pause` entrypoint has been overridden.

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

