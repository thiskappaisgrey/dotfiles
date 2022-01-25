{  pkgs ? import <nixpkgs> {}, fetchgit ? pkgs.fetchgit, stdenv ? pkgs.stdenv, ... }:

stdenv.mkDerivation {
  name = "pokemon-colorscripts";
  src = pkgs.fetchFromGitLab {
    owner = "phoneybadger";
    repo = "pokemon-colorscripts";
    rev = "05d69994413e9faee13720da040cae68d2821791";
    sha256 = "0vjqzh4pcnwv806sgj37gav9dl9p516zspaad6x96bf46ji71dpn";
  };
  installPhase = ''
          mkdir -p $out/bin
          mv colorscripts $out/bin
          mv pokemon-colorscripts.sh $out/bin/pokemon-colorscripts
          mv nameslist.txt $out/bin
      '';

}
