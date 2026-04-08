{
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskell-nix }: flake-utils.lib.eachDefaultSystem (system:
    let
      project = (import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay
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
        inherit (haskell-nix) config;
      }).hixProject;
    in
    project.flake { } // { packages.lifx-manager = project.hsPkgs.lifx-manager.components.exes.lifx-manager; });
}
