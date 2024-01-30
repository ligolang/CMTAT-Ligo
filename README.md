# CMTA Token

This repository is an implementation in cameligo of a CMTA token described by the [specifications](https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf).

It includes features from the basic token standard (TZIP12) and extra features for managing the total supply, for providing snapshots, and granting authorization to other users.

This implementation is architectured as a library which can be used to create a CMTA token. 
This library is extendable which means that when creating a CMTA token, it is possible to override features and to extend the storage with extra fields. These extra features can rely on extra fields in the storage and thus allow to create custom token with CMTA features and additionnal custom features.

This library relies on the `@ligo/fa` library which implements the TZIP-12 standard features and provides an implementation for fungible single-asset, fungible multi-asset , and non-fungible token. In the same manner the CMTA Token library also supports these 3 kind of tokens.


## Modules

The CMTAT library provides features separated on modules 
FA2 basic features (from standard) consists on allowing people to transfer assets between them  


The *Totalsupply* module consist on keep track of the total supply of the token.
The *Administration* module consist on allowing a special user (administrator) to pause/unpause the contract, and to create assets ad to destroy assets. 
The *Authorizations* module allows the administrator to grant administration roles to other user.
The *Snapshots* module keep track of total supply and account balance at certain point in time.

## Entrypoints




## Tests

```
make test
```
