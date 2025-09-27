{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    name = "Chrobelias";
    packages = with pkgs; [ 
      gmp
      opam

      llvm_18
      zlib
      libtinfo
    ];
}
