{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
    }:
    let
      theseHpkgNames = [ "eslint-fix-import-extensions" ];
      thisGhcVersion = "ghc96";
      # # Only required for projects with multiple packages.
      # hMkPackage = h: n: h.callCabal2nix n (./. + "/${n}") { };
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides =
            selfh: superh:
            supern.haskell.packageOverrides selfh superh
            // {
              eslint-fix-import-extensions = selfh.callCabal2nix "eslint-fix-import-extensions" ./. { };
            };
        };
      };
      overlays = [ hOverlay ];
      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = theseHpkgs // {
            default = theseHpkgs.eslint-fix-import-extensions;
          };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = [ ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;
    }
    // flake-utils.lib.eachDefaultSystem perSystem;
}
