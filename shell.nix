let nixos = fetchTarball { 
  url = "https://releases.nixos.org/nixos/20.09/nixos-20.09.3505.12d9950bf47/nixexprs.tar.xz";
  sha256 = "0fsl8bsdb8i536pfs4wrp0826h5l84xqlwx32sbz66jg4ykqp9lr";
}; in

with (import nixos {});

let
  ghc = haskell.packages.ghc8102.ghcWithPackages (_: []);
in

stdenv.mkDerivation {
  name = "build-shell";
  buildInputs = [
    cabal-install
    # mesa_drivers
    # mesa_noglu.drivers
    libGL
    # mesa_glu
    libGLU
    # libGL_driver
    glxinfo
    git
    freeglut
    zlib
    haskell.compiler.ghc8101
    # ghcide-nix.ghcide-ghc865
    # vscode
  ];
  shellHook = ''
    export PATH="$PATH:$PWD/bin"
  '';
  # I wish we could do without this ugly hack.
  LD_LIBRARY_PATH = with pkgs; "${libGL}/lib:${mesa_glu}/lib:${freeglut}/lib";
}
