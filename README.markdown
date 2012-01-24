# 12UrenLoop

People run laps. We count, give them points and monitor it all.

## count-von-count

Building the application:

1. Install [Haskell Platform](http://hackage.haskell.org/platform/)
2. Run `cabal update`
3. From the count-von-count folder, run `cabal install --only-dependencies`
4. Execute `cabal build`

The executable is now available as `dist/build/count-von-count/count-von-count`.

Developing the application:

1. Run `ghci` from the project folder
2. Load the main source file: `:l src/CountVonCount/Main.hs`
3. Enter `main` to start the program, `^C` to interrupt
4. Enter `:r` to reload all changed modules

Testing the application: ???

## pokemon

Demo application that generates detection events and sends them to a
count-von-count process. Requires [LÖVE](https://love2d.org/) to run.

## tools/data-analysis

Checks the gathered data for inconsistencies.

## tools/monitor.rb

Runs a series of checks on each monitored host.

## tools/macalive.sh

Checks if a bluetooth device is still alive using the hcitool command. Used
to assess the lifetime of our batons.
