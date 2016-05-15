{ mkDerivation, aeson, base, base-unicode-symbols, bytestring
, HsOpenSSL, http-client, http-client-openssl, lens, mtl, parsers, QuickCheck
, safe, scientific, split, stdenv, text, time, trifecta, unordered-containers
, utf8-string, vector, wreq
###
, with-cabal-install ? false, cabal-install
}:
mkDerivation {
  pname = "youtrack";
  version = "0.0.6";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base-unicode-symbols bytestring HsOpenSSL http-client
    http-client-openssl lens mtl parsers QuickCheck safe scientific text split time trifecta
    unordered-containers utf8-string vector wreq
  ] ++ (if with-cabal-install then [ cabal-install ] else []);
  homepage = "https://github.com/deepfire/youtrack";
  description = "Access a Jetbrains YouTrack instance";
  license = stdenv.lib.licenses.gpl3;
}
