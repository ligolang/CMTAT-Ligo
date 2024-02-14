import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { char2Bytes } from "@taquito/utils";

import singleAssetContract from "../compiled/example/extended_cmtat_single_asset.mligo.json";

const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";

async function main() {
  const Tezos = new TezosToolkit(RPC_ENDPOINT);

  //set alice key
  Tezos.setProvider({
    signer: await InMemorySigner.fromSecretKey(
      "edskS7YYeT85SiRZEHPFjDpCAzCuUaMwYFi39cWPfguovTuNqxU3U9hXo7LocuJmr7hxkesUFkmDJh26ubQGehwXY8YiGXYCvU"
    ),
  });

  const ledger = new MichelsonMap();
  ledger.set("tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2", 100);

  const token_metadata = new MichelsonMap();
  const token_info = new MichelsonMap();
  token_info.set("name", char2Bytes("CMTAT_EXAMPLE"));
  token_info.set("description", char2Bytes("My custom extended CMTA Token (single asset)"));
  token_info.set("symbol", char2Bytes("XXX"));
  token_info.set("decimals", char2Bytes("0"));
  token_info.set("ISIN", char2Bytes("US0378331005"));
  token_info.set("terms", char2Bytes("https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf"));

  token_metadata.set(0, { token_id: 0, token_info });

  const metadata = new MichelsonMap();
  metadata.set("", char2Bytes("tezos-storage:data"));
  metadata.set(
    "data",
    char2Bytes(`{
    "name":"CMTAT_TEST",
    "description":"Example CMTA Token implementation",
    "version":"0.1.0",
    "license":{"name":"MIT"},
    "authors":["Frank Hillard<frank.hillard@gmail.com>"],
    "homepage":"",
    "source":{"tools":["Ligo"], "location":"https://github.com/ligolang/CMTAT-Ligo/example"},
    "interfaces":["TZIP-012"],
    "errors":[],
    "views":[]
  
  }`)
  );

  const operators = new MichelsonMap();

  const administration = { 
    admin : "tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2",
    paused : false,
    killed : false 
  };
  const totalsupplies  = 100;
  const authorizations = new MichelsonMap();
  const snapshots = {
    account_snapshots : new MichelsonMap(),
    totalsupply_snapshots : new MichelsonMap(),
    scheduled_snapshots : [],
  };
  // const validation = {
  //   rule_engine_contract : null
  // };
  const validation = null;

  // const extension = {
  //   issuer : "tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2"
  // }
  const extension = "tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2";



  const initialStorage = {
    ledger,
    metadata,
    token_metadata,
    operators,
    administration,
    totalsupplies,
    authorizations,
    snapshots,
    validation,
    extension
  };

  try {
    const originated = await Tezos.contract.originate({
      code: singleAssetContract,
      storage: initialStorage,
    });
    console.log(
      `Waiting for singleAssetContract ${originated.contractAddress} to be confirmed...`
    );
    await originated.confirmation(2);
    console.log("confirmed contract: ", originated.contractAddress);
  } catch (error: any) {
    console.log(error);
  }
}

main();
