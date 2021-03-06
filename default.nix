{ stdenv, mkDerivation
, aeson, base, base-unicode-symbols, bytestring
, HsOpenSSL, http-client, http-client-openssl, lens, mtl, optparse-applicative
, parsers, pretty-show, QuickCheck, safe, scientific, split, text, time, trifecta
, unicode-show, unordered-containers, utf8-string, vector, wreq
}:
mkDerivation {
  pname = "youtrack";
  version = "0.0.8";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base-unicode-symbols bytestring
    HsOpenSSL http-client http-client-openssl lens mtl optparse-applicative
    parsers pretty-show QuickCheck safe scientific split text split time trifecta
    unicode-show unordered-containers utf8-string vector wreq
  ];
  homepage = "https://github.com/deepfire/youtrack";
  description = "Access a Jetbrains YouTrack instance";
  license = stdenv.lib.licenses.gpl3;
}
