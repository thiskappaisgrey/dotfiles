{ nixpkgs ? import <nixpkgs> { } }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  # add taffybar here so I can just spawn one hoogle instance when I want to hack on either.. Prob want to figure out another way to do it at some point potentially? Or if this works just stick with it lol
  haskellDeps = ps: with ps; [ base xmonad xmonad-contrib xmonad-extras  ];

  ghc = haskellPackages.ghcWithHoogle haskellDeps;

  nixPackages = [ ghc haskellPackages.cabal-install haskellPackages.brittany haskellPackages.hlint haskellPackages.ghcid ];
in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
