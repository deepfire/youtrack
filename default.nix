{ mkDerivation, aeson, base, base-unicode-symbols, bytestring
, HsOpenSSL, http-client, http-client-openssl, lens, mtl, parsers
, safe, scientific, stdenv, text, time, trifecta, unordered-containers
, utf8-string, vector, wreq
}:
mkDerivation {
  pname = "youtrack";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base-unicode-symbols bytestring HsOpenSSL http-client
    http-client-openssl lens mtl parsers safe scientific text time trifecta
    unordered-containers utf8-string vector wreq
  ];
  homepage = "https://github.com/deepfire/youtrack";
  description = "Access a Jetbrains YouTrack instance";
  license = stdenv.lib.licenses.gpl3;
}
