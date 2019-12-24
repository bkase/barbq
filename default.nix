{ compiler ? "ghc865", pkgs ? import <nixpkgs> {} }:

let
  getvolume = pkgs.callPackage ./getvolume/default.nix { };
  packageSet = pkgs.haskell.packages.${compiler};
  haskellPackages =
    packageSet.override {
              overrides = (self: super:
                {
                  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                  ghcWithPackages = self.ghc.withPackages;
                  async-timer = pkgs.haskell.lib.dontCheck super.async-timer;
                }
              );
            };
            raw = haskellPackages.callCabal2nix "barbq" ./. {};
            drv = pkgs.symlinkJoin {
              name = "barbq";
              paths = [ raw getvolume  ];
            };

in
  {
    barbq = drv;
    barbq-shell = haskellPackages.shellFor {
      packages = p: [raw];
      shellHook = ''
        export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
      '';
      buildInputs = with pkgs; [ cabal-install hlint ghcid getvolume ] ;
    };
  }

