
# Compilation

1.  Install Haskell toolchain (<https://www.haskell.org/ghcup/>, I use GHC 9.6.5
    and `cabal`).

2.  Compile:
    
        cabal build
        cabal install
    
    Check that the path of the installed binary is in your `PATH`.


# Usage

    pnpm run lint | fix-imports

