12UrenLoop
==========

People run laps. We count and shit like that.

count-von-count
---------------

Backend. Receives bluetooth measurements from another component, and analyzes
the data in order to count laps.

### Installing & running

Install [cabal-install](http://www.haskell.org/cabal/), the easiest way is to
use your platform's package manager.

Compile `count-von-count`:

    cabal configure
    cabal build

Installation (optional):

    cabal install

Installation is not strictly necessary, you can also run it as

    ./dist/build/count-von-count/count-von-count

Run the tests:

    runghc -isrc -itests tests/TestSuite.hs

dr.beaker
---------

Presents the data analyzed by `count-von-count` in a nice fashion.
