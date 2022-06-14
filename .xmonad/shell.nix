{ nixpkgs ? import <nixpkgs> { } }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  # add taffybar here so I can just spawn one hoogle instance when I want to hack on either.. Prob want to figure out another way to do it at some point potentially? Or if this works just stick with it lol
  haskellDeps = ps: with ps; [ base xmonad_0_17_0 xmonad-contrib_0_17_0 xmonad-extras_0_17_0 taffybar ];

  ghc = haskellPackages.ghcWithHoogle haskellDeps;

  nixPackages = [ ghc pkgs.gdb haskellPackages.cabal-install haskellPackages.brittany ];
in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
