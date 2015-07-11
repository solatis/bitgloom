{ mkDerivation, async-pool, base, bitcoin-types, classy-prelude
, classy-prelude-yesod, crypto-numbers, crypto-random, hspec
, hspec-expectations, mtl, network, persistent, persistent-template
, stdenv, text, transformers
}:
mkDerivation {
  pname = "bitgloom-driver";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    async-pool base bitcoin-types classy-prelude classy-prelude-yesod
    crypto-numbers crypto-random mtl persistent persistent-template
    text transformers
  ];
  testDepends = [
    base crypto-random hspec hspec-expectations network
  ];
  license = stdenv.lib.licenses.unfree;
}
