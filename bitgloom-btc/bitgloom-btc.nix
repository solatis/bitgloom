{ mkDerivation, base, base32string, binary, bitcoin-api
, bitcoin-api-extra, bitcoin-script, bitcoin-tx, bitcoin-types
, bytestring, conduit, conduit-combinators, exceptions, hexstring
, hspec, hspec-expectations, http-client, http-types, lens, network
, stdenv, text, transformers
}:
mkDerivation {
  pname = "bitgloom-btc";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    base binary bitcoin-api bitcoin-api-extra bitcoin-script bitcoin-tx
    bitcoin-types bytestring conduit conduit-combinators exceptions
    http-client http-types lens text transformers
  ];
  testDepends = [
    base base32string bitcoin-api bytestring conduit
    conduit-combinators hexstring hspec hspec-expectations network text
  ];
  license = stdenv.lib.licenses.unfree;
}
