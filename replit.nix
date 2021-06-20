{ pkgs }: {
    deps = with pkgs; [
      libGL
      libGLU
      cabal-install
      ghc
      stack
    ];
}

