# thrust-gen
A Synthesizer for the [Thrust C++-based DSL](https://developer.nvidia.com/Thrust).

## Building

This should be as simple as:

```
git clone https://github.com/ku-fpg/thrust-gen
cd thrust-gen/
cabal build
cabal build ionize
cabal install
```

## General Usage
There is a tool, called ```ionize``` which can be used to 
run your haskell code, generating a .cu file, and then optionally
run that .cu file.

Here are the base commands.

```ionize help``` Gives options
```ionize run file.hs``` Will build a new directory, generate the .cu source
and place it there, then generate the thrust executable and run it.
```ionize build file.hs``` Will do the same as run without running it.

### Examples
There are multiple examples in the ```examples``` directory, showing
the ion versions, as well as accelerate and thrust.
