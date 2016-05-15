{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7103"
, with-cabal-install ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.; # your default.nix

  haskell             = pkgs.haskell;
  haskellPackagesOrig = haskell.packages.${compiler};

  haskellPackages     = haskellPackagesOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
      # aeson      = haskell.packages.ghc7103.aeson;
      # lens-aeson = haskell.packages.ghc7103.lens-aeson;
      # wreq       = haskell.packages.ghc7103.wreq;
    };
  });

  drv = haskellPackages.callPackage f {
    with-cabal-install = with-cabal-install;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
