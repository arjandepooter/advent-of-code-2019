{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "advent-of-code2019";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers ];
  license = stdenv.lib.licenses.mit;
}
