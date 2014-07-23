This package contains the Scheme programming language, compiler and interactive environment, implemented in Haskell.

To compile, you need a Haskell environment and include make and some packages, like:

ghc --make -package parsec -fglasgow-exts -o glasgow_scheme ten_scheme.hs 
