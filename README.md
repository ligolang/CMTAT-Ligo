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


## Modules

The CMTAT library provides features separated on modules which can be used in the final token contract. 
FA2 basic features (from standard) consists on allowing people to transfer assets between them  


The *Totalsupply* module consists on keep track of the total supply of the token.
The *Administration* module consists on allowing a special user (administrator) to pause/unpause the contract, and to create assets ad to destroy assets. 
The *Authorizations* module allows the administrator to grant administration roles to other user.
The *Snapshots* module keeps track of total supply and account balance at certain point in time.


## Token



### Entrypoints





## Tests

An exemple of CMTA Token contract has been implemented for testing purposes. This [extended_cmtat_single_asset] (./test/cmtat/extended_cmtat_single_asset.instance.mligo) illustrates 
- how to implement a token contract based on cmtat library 
- how to define entrypoints using the default behavior
- how to customize the storage of the token contract
- how to customize entrypoints while reusing sub-modules of the library

This token contract is used to originate a contract instance and to run test on related entrypoints. These tests of the [extended_cmtat_single_asset] (./test/cmtat/extended_cmtat_single_asset.test.mligo) illusrates how to execute entrypoints in predefined conditions and how to verify execution termination (success and failures).

These tests also ensures that the default implementation provided by the library is doing what is expected of it !

To run these tests, run the following command
```
make test
```
