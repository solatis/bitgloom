{ mkDerivation, async, async-pool, base, bitgloom-driver
, classy-prelude, hspec, hspec-expectations, monad-logger, network
, persistent, persistent-sqlite, stdenv, text, transformers
}:
mkDerivation {
  pname = "bitgloom-worker";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    async async-pool base classy-prelude persistent text
  ];
  testDepends = [
    base bitgloom-driver hspec hspec-expectations monad-logger network
    persistent persistent-sqlite transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
