let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskellPackages; 
  project = pkgs.haskellPackages.callPackage ./default.nix { };
in
  pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = project.env.nativeBuildInputs ++ [
      haskellPackages.ghcid
    ];
  }
