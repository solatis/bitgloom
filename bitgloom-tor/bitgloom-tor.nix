{ mkDerivation, base, exceptions, hspec, hspec-expectations
, network, network-anonymous-tor, stdenv, transformers
}:
mkDerivation {
  pname = "bitgloom-tor";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    base exceptions network-anonymous-tor transformers
  ];
  testDepends = [ base hspec hspec-expectations network ];
  license = stdenv.lib.licenses.unfree;
}
