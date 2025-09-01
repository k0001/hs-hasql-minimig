{ mkDerivation, base, containers, hasql, hasql-transaction, lib
, text, time
}:
mkDerivation {
  pname = "hasql-minimig";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers hasql hasql-transaction text time
  ];
  testHaskellDepends = [ base hasql hasql-transaction ];
  homepage = "https://github.com/k0001/hs-hasql-minimig";
  description = "Forward-only list-based migrations for Hasql";
  license = lib.licenses.asl20;
}
