{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_8" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.;

  haskell  = pkgs.haskell;
  ghcOrig  = pkgs.haskell.packages.${compiler};

  ghc      = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
      aeson          = old.aeson_0_11_1_1;
    };
  });

  drv = ghc.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
