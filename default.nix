{ mkDerivation, aeson, base, directory, extensible, extra
, file-embed, graphql-parser, lib, template-haskell, text
}:
mkDerivation {
  pname = "graphql-extensible-records";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base directory extensible extra file-embed graphql-parser
    template-haskell text
  ];
  testHaskellDepends = [
    aeson base directory extensible extra file-embed graphql-parser
    template-haskell text
  ];
  homepage = "https://github.com/charlescrain/graphql-extensible-records#readme";
  license = lib.licenses.bsd3;
}
