"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const signer_1 = require("@taquito/signer");
const taquito_1 = require("@taquito/taquito");
const utils_1 = require("@taquito/utils");
const cmtat_single_asset_impl_mligo_json_1 = __importDefault(require("../compiled/cmtat/asset/cmtat_single_asset.impl.mligo.json"));
const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        const Tezos = new taquito_1.TezosToolkit(RPC_ENDPOINT);
        //set alice key
        Tezos.setProvider({
            signer: yield signer_1.InMemorySigner.fromSecretKey("edskS7YYeT85SiRZEHPFjDpCAzCuUaMwYFi39cWPfguovTuNqxU3U9hXo7LocuJmr7hxkesUFkmDJh26ubQGehwXY8YiGXYCvU"),
        });
        const ledger = new taquito_1.MichelsonMap();
        ledger.set("tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2", 100);
        const token_metadata = new taquito_1.MichelsonMap();
        const token_info = new taquito_1.MichelsonMap();
        token_info.set("name", (0, utils_1.char2Bytes)("CMTAT_TOKEN_0"));
        token_info.set("description", (0, utils_1.char2Bytes)("My CMTA Token (single asset)"));
        token_info.set("symbol", (0, utils_1.char2Bytes)("XXX"));
        token_info.set("decimals", (0, utils_1.char2Bytes)("0"));
        token_info.set("ISIN", (0, utils_1.char2Bytes)("US0378331005"));
        token_info.set("terms", (0, utils_1.char2Bytes)("https://cmta.ch/content/15de282276334fc837b9687a13726ab9/cmtat-functional-specifications-jan-2022-final.pdf"));
        token_metadata.set(0, { token_id: 0, token_info });
        const metadata = new taquito_1.MichelsonMap();
        metadata.set("", (0, utils_1.char2Bytes)("tezos-storage:data"));
        metadata.set("data", (0, utils_1.char2Bytes)(`{
    "name":"CMTAT_DEFAULT_SINGLE",
    "description":"Example CMTA Token (default single asset configuration)",
    "version":"0.1.0",
    "license":{"name":"MIT"},
    "authors":["Frank Hillard<frank.hillard@gmail.com>"],
    "homepage":"",
    "source":{"tools":["Ligo"], "location":"https://github.com/ligolang/CMTAT-Ligo/example"},
    "interfaces":["TZIP-012"],
    "errors":[],
    "views":[]
  
  }`));
        const operators = new taquito_1.MichelsonMap();
        const administration = {
            admin: "tz1TiFzFCcwjv4pyYGTrnncqgq17p59CzAE2",
            paused: false,
            killed: false
        };
        const totalsupplies = 100;
        const authorizations = new taquito_1.MichelsonMap();
        const snapshots = {
            account_snapshots: new taquito_1.MichelsonMap(),
            totalsupply_snapshots: new taquito_1.MichelsonMap(),
            scheduled_snapshots: [],
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
            const originated = yield Tezos.contract.originate({
                code: cmtat_single_asset_impl_mligo_json_1.default,
                storage: initialStorage,
            });
            console.log(`Waiting for singleAssetContract ${originated.contractAddress} to be confirmed...`);
            yield originated.confirmation(2);
            console.log("confirmed contract: ", originated.contractAddress);
        }
        catch (error) {
            console.log(error);
        }
    });
}
main();
