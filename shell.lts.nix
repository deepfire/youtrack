{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_11" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.;

  haskell  = pkgs.haskell;
  ghcOrig  = pkgs.haskell.packages.${compiler};

  ghc      = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
    };
  });

  drv = ghc.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
