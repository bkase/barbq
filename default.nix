{ compiler ? "ghc865", pkgs ? import <nixpkgs> {} }:

let

  packageSet = pkgs.haskell.packages.${compiler};
  haskellPackages =
    packageSet.override {
              overrides = (self: super:
                {
                  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                  ghcWithPackages = self.ghc.withPackages;

                  polysemy = self.polysemy_1_2_1_0;

                  async-timer = pkgs.haskell.lib.dontCheck super.async-timer;

                  polysemy-plugin = pkgs.haskell.lib.dontCheck (super.polysemy-plugin.override { polysemy = self.polysemy_1_2_1_0; });
                }
              );
            };
  drv = haskellPackages.callCabal2nix "barbq2" ./. {};

in
  {
    barbq = drv;
    barbq-shell = haskellPackages.shellFor {
      packages = p: [drv];
      shellHook = ''
        export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
      '';
      buildInputs = with pkgs; [ cabal-install hlint ghcid];
    };
  }

