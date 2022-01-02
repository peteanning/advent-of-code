# Advent of Code 2021 In Haskell

To run the tests you will need HUnit. I have used `cabal` to [install](https://gist.github.com/peteanning/e3cb8c9aef4318cbaf46d5e1abf3d06a) the HUnit
dependencies. 

There are a few more dependencies specified in the cabal files, notably:
- split
- containers ( Data.Map Data.Set )

## Testing

`cabal v2-test all`

or 

`cabal v2-test day<n>`
