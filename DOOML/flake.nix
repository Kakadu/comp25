{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }: 
        flake-utils.lib.eachDefaultSystem (system: 
            let 
                pkgs = nixpkgs.legacyPackages.x86_64-linux;
                riscv-cross-pkgs = import nixpkgs {
                  localSystem = "${system}";
                  crossSystem = {
                    config = "riscv64-linux-gnu";
                  };
                };
            in {
                packages.dooml = pkgs.stdenv.mkDerivation {
                    name = "dooml";
                    version = "0.0";
                    src = ./.;
                    buildInputs = with pkgs; [
                    ];
                    buildPhase = ''
                        ls
                    '';
                    installPhase = ''mkdir $out'';
                };

                packages.default = self.packages.${system}.dooml;
                devShell = pkgs.mkShell {
                    name = "dooml";
                    packages = with pkgs; [
                      # Lsp, treesitter-parsers and debugger
                      ocamlformat_0_28_1

                      # Build tools
                      opam

                      # Dependencies
                      gmp
                      llvm_19
                      zlib
                      libtinfo
                    ] ++ [ riscv-cross-pkgs.buildPackages.gcc ];
                    shellHook = ''
                      eval $(opam env)
                    '';
                };
            });
}
