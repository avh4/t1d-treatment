module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Bayes


assertMatches : List (a,Float) -> Bayes.DiscreteDistribution a -> Assertion
assertMatches expected dist =
    let
        actual = List.map (fst >> \a -> (a, Bayes.get a dist)) expected
    in
        assertEqual expected actual


all : Test
all =
  suite "Stats.prior"
    [
      Bayes.uniformPrior1 [0..3]
      |> assertMatches [(0,0.25), (1,0.25), (2,0.25), (3,0.25)]
      |> test "1-dimensional discrete uniform distribution",

      Bayes.uniformPrior3 (,,) [0,1] [2,3] [4,5]
      |> assertMatches
          [ ((0,2,4),0.125), ((0,2,5),0.125)
          , ((0,3,4),0.125), ((0,3,5),0.125)
          , ((1,2,4),0.125), ((1,2,5),0.125)
          , ((1,3,4),0.125), ((1,3,5),0.125)
          ]
      |> test "3-dimensional discrete uniform distribution",

      Bayes.uniformPrior2 (,) [0,1] [2,3]
      |> Bayes.marginal (\(a,b) -> b)
      |> assertMatches
          [ (2,0.5), (3,0.5) ]
      |> test "calculate marginal distributions"
    ]
