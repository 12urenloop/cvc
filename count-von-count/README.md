# count-von-count

## Fix your Haskell

First of all, fix your Haskell by installing [Stack](https://docs.haskellstack.org/en/stable/README/). (You can use your own package manager, ex. `sudo xbps-install stack`)

Next, install cabal-install with `stack install cabal-install`.

**When you are running or installing `cabal` or `ghci` manually (without stack), you're doing it wrong.**

## Dependencies

Stack will fix this for you.

## Usage

To build the application run `stack build`. The executable is now available somewhere in the depths of `.stack-work/...`. You can install the cvc executable locally with `stack install`.

### Developing the application:

1. Run `stack ghci` from the project folder
2. Select the main package (option 1: `src/CountVonCount.hs`)
3. Load the main source file: `:l src/CountVonCount/Main.hs`
4. Enter `main` to start the program, `^C` to interrupt
5. Enter `:r` to reload all changed modules

### Testing the application:

```
stack test
```

### Scripts

Running files in the `scripts/` directory when using a sandbox:

Run `stack runghc scripts/teams2017.hs`.

OR

1. Run `stack ghci` from the project folder
2. Load the scripts (for instance `scripts/teams2015`) with `:l
   scripts/teams2017.hs`
3. Run the script with `main`

## Other tools

* count-von-count-simulation
* count-von-count-replayer
