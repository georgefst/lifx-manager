{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }: flake-utils.lib.eachDefaultSystem (system:
    let project = (import nixpkgs {
      inherit system;
      overlays = [
        haskellNix.overlay
        (final: _prev: {
          hixProject = final.haskell-nix.hix.project {
            src = ./.;
            compiler-nix-name = "ghc912";
            cabalProjectFreeze = "";
            shell.tools.cabal = "latest";
            shell.tools.haskell-language-server = "latest";
          };
        })
      ];
      inherit (haskellNix) config;
    }).hixProject; in project.flake { } // { packages.lifx-manager = project.hsPkgs.lifx-manager.components.exes.lifx-manager; });
}
