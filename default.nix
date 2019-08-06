
let
  opts = {
    # packageOverrides = pkgs: rec {
    #   haskellPackages = pkgs.haskellPackages.override {
    #     overrides = haskellPackagesNew: haskellPackagesOld: rec {
    #       insert-ordered-containers = pkgs.haskell.lib.doJailbreak haskellPackagesOld.insert-ordered-containers;
    #       these = pkgs.haskell.lib.doJailbreak haskellPackagesOld.these;
    #       HaTeX = pkgs.haskell.lib.doJailbreak haskellPackagesOld.HaTeX;
    #       wl-pprint-extras = pkgs.haskell.lib.doJailbreak haskellPackagesOld.wl-pprint-extras;
    #     };
    #   };
    # };
  };

  pkgsPath = (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/b3a88b8d10625e097d6da9cb6a7dca797f9f8f8e.tar.gz;
    sha256 = "0fr52cs39vpx7b5vbhm3a18rl2c8xsbri2w6d1abqwlmmyhv1rxc";
  });

  pkgs = import pkgsPath { config = opts; };
in

pkgs.stdenv.mkDerivation {
  name = "test";
  src = ./.;
  phases = ["installPhase"];
  installPhase =
    ''
    echo Please use nix-shell!
    touch $out
    '';
  buildInputs = [
    pkgs.gmp
    pkgs.zlib
    # pkgs.libiconv
    pkgs.curl
    pkgs.cmake
    # pkgs.gnupg
    pkgs.python
    pkgs.nodejs
    pkgs.stack
    pkgs.nix

    # (
    # pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs;
    #     [ parsers
    #       trifecta
    #       text
    #     ])
    # )
   ];
  shellHook = ''
    export NIX_PATH="nixpkgs=${pkgsPath}"
  '';
}

