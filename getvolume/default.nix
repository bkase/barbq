{pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  name = "barbq-getvolume";
  version = "0.2";
  src = ./.;
  buildPhase = ''
    clang main.c -framework CoreAudio -framework AudioToolbox
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv a.out $out/bin/barbq-getvolume
  '';
  buildInputs = [ pkgs.darwin.apple_sdk.frameworks.CoreAudio pkgs.darwin.apple_sdk.frameworks.AudioToolbox ];
}
