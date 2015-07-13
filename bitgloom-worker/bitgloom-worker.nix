{ mkDerivation, async-pool, base, classy-prelude, hspec
, hspec-expectations, persistent, stdenv, text
}:
mkDerivation {
  pname = "bitgloom-worker";
  version = "0.0.1";
  src = ./.;
  buildDepends = [ async-pool base classy-prelude persistent text ];
  testDepends = [ base hspec hspec-expectations ];
  license = stdenv.lib.licenses.unfree;
}
