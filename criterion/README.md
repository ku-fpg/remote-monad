How to run the benchmarks
=========================

Inside the root directory of the project, run

    % cabal configure --enable-benchmarks && cabal build && cabal bench

After benchmarks are built, you can run the html generator

    % ./dist/build/remote-monad-bench/remote-monad-bench -o X.html

