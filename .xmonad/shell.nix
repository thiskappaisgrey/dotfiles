{ nixpkgs ? import <nixpkgs> { } }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [ base xmonad xmonad-contrib ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [ ghc pkgs.gdb haskellPackages.cabal-install haskellPackages.brittany ];
in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
