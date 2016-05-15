{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
, with-cabal-install ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.; # your default.nix

  haskell             = pkgs.haskell;
  haskellPackages     = haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {
    with-cabal-install = with-cabal-install;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
