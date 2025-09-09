{ mkDerivation, base, bytestring, containers, directory, filepath
, hasql, hasql-transaction, lib, parsec, text, time
}:
mkDerivation {
  pname = "hasql-minimig";
  version = "0.1";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath hasql
    hasql-transaction parsec text time
  ];
  testHaskellDepends = [ base filepath hasql hasql-transaction ];
  homepage = "https://github.com/k0001/hs-hasql-minimig";
  description = "Forward-only list-based migrations for Hasql";
  license = lib.licenses.asl20;
}
