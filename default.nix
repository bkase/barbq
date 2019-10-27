{ compiler ? "ghc865", pkgs ? import <nixpkgs> {} }:

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "barbq2" ./. {};

in
  {
    barbq = drv;
    barbq-shell = haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ cabal-install hlint ghcid];
    };
  }

