NIX_SHELL := nix-shell --max-jobs 8 --cores 8 --no-build-output
CORR      := dist/build/corr/corr
HSDEPS    := $(wildcard *.hs)

all: yt

###
###
###
lts-sh:
	$(NIX_SHELL) \
	  shell.lts.nix \
          --arg with-cabal-install true

###
###
###
clean:
	cabal clean
yt:
	cabal build
