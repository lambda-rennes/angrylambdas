#let nixos = fetchTarball { 
#  url = "https://releases.nixos.org/nixos/20.09/nixos-20.09.3505.12d9950bf47/nixexprs.tar.xz";
#  sha256 = "0fsl8bsdb8i536pfs4wrp0826h5l84xqlwx32sbz66jg4ykqp9lr";
#}; in

let pkgsSrc = fetchTarball "https://github.com/NixOS/nixpkgs/archive/c40c611ff9e58a73c2cc277a047117d84082ec8d.tar.gz"; in

with (import pkgsSrc {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (_: []);
in

stdenv.mkDerivation {
  name = "build-shell";
  buildInputs = [
    cabal-install
    libGL
    libGL.dev
    libGLU
    glxinfo
    git
    gmp.dev
    libffi.dev
    freeglut
    zlib
    haskell.compiler.ghc8104
    stack
  ];
  shellHook = with pkgs; ''
    export PATH="$PATH:$PWD/bin"
  '';
  # I wish we could do without this ugly hack.
  LD_LIBRARY_PATH = "${libGL}/lib:${mesa_glu}/lib:${freeglut}/lib:${gmp}/lib:${libffi}/lib";
}
