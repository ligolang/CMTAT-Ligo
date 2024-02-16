import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { char2Bytes } from "@taquito/utils";

import singleAssetContract from "../compiled/cmtat/asset/cmtat_multi_asset.impl.mligo.json";

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
  ledger.set(["tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2", 1], 100);
  ledger.set(["tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2", 2], 100);

  const token_metadata = new MichelsonMap();
  const token_info_1 = new MichelsonMap();
  token_info_1.set("name", char2Bytes("CMTAT_TOKEN_1"));
  token_info_1.set("description", char2Bytes("My Token XXX"));
  token_info_1.set("symbol", char2Bytes("XXX"));
  token_info_1.set("decimals", char2Bytes("0"));
  token_info_1.set("ISIN", char2Bytes("US0378331000"));
  token_info_1.set("terms", char2Bytes("https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf"));
  const token_info_2 = new MichelsonMap();
  token_info_2.set("name", char2Bytes("CMTAT_TOKEN_2"));
  token_info_2.set("description", char2Bytes("My Token YYY"));
  token_info_2.set("symbol", char2Bytes("YYY"));
  token_info_2.set("decimals", char2Bytes("0"));
  token_info_2.set("ISIN", char2Bytes("US0378331999"));
  token_info_2.set("terms", char2Bytes("https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf"));

  token_metadata.set(1, { token_id: 1, token_info: token_info_1 });
  token_metadata.set(2, { token_id: 2, token_info: token_info_2 });

  const metadata = new MichelsonMap();
  metadata.set("", char2Bytes("tezos-storage:data"));
  metadata.set(
    "data",
    char2Bytes(`{
    "name":"CMTAT_DEFAULT_MULTI",
    "description":"Example CMTA Token (default multi asset configuration)",
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

  const totalsupplies = new MichelsonMap();
  totalsupplies.set(1, 100);
  totalsupplies.set(2, 100);

  const administration = { 
    admin : "tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2",
    paused : false,
    killed : false 
  };

  const authorizations = {
    general : new MichelsonMap(),
    specific : new MichelsonMap(),
  };
  const snapshots = {
    account_snapshots : new MichelsonMap(),
    totalsupply_snapshots : new MichelsonMap(),
    scheduled_snapshots : [],
  };
  const validation = null;

  const initialStorage = {
    ledger,
    metadata,
    token_metadata,
    operators,
    administration,
    totalsupplies,
    authorizations,
    snapshots,
    validation
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
