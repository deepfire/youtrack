{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.; # your default.nix

  haskell             = pkgs.haskell;
  haskellPackagesOrig = haskell.packages.${compiler};

  haskellPackages     = import ../nixpkgs-haskellpackages-overrides/ghc801.nix
                          pkgs haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
