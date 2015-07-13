{ mkDerivation, aeson, attoparsec, base, bifunctors, bitcoin-types
, bitgloom-btc, bitgloom-driver, bitgloom-tor, bytestring
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, containers, data-default, directory, fast-logger
, file-embed, hjsmin, http-conduit, monad-control, monad-logger
, mtl, persistent, persistent-sqlite, persistent-template, safe
, shakespeare, stdenv, template-haskell, text, time
, unordered-containers, vector, wai-extra, wai-logger, warp, yaml
, yesod, yesod-core, yesod-form, yesod-persistent, yesod-static
}:
mkDerivation {
  pname = "bitgloom-www";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bifunctors bitcoin-types bitgloom-btc
    bitgloom-driver bitgloom-tor bytestring classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed hjsmin http-conduit
    monad-control monad-logger mtl persistent persistent-sqlite
    persistent-template safe shakespeare template-haskell text time
    unordered-containers vector wai-extra wai-logger warp yaml yesod
    yesod-core yesod-form yesod-persistent yesod-static
  ];
  license = stdenv.lib.licenses.unfree;
}
