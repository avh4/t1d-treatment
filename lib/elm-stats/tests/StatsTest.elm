module StatsTest where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Stats


all : Test
all =
  suite "Stats"
    [
      [-1, 0, 1]
      |> List.map (Stats.normal 0 1).pdf
      |> assertEqual [0.24197072451914337,0.3989422804014327,0.24197072451914337]
      |> test "normal distribution probability density function",

      [ 600, 470, 170, 430, 300 ]
      |> Stats.stddev
      |> assertEqual 164.7118696390761
      |> test "standard deviation",

      [ 1,2,3,4,5 ]
      |> Stats.stddev
      |> assertEqual 1.5811388300841898
      |> test "standard deviation",

      [ 0,1 ]
      |> Stats.stddev
      |> assertEqual 0.7071067811865476
      |> test "standard deviation",

      [1,2,3,4,5]
      |> Stats.bins
      |> assertEqual [(1,4),(4.236294590020318,1)]
      |> test "trivial binning",

      [1,2]
      |> Stats.bins
      |> assertEqual [(1,2)]
      |> test "trivial binning fits the range",

      [1,2,3,5,6]
      |> Stats.bins
      |> assertEqual [(1,4),(5.244360564117875,1)]
      |> test "simple binning"
    ]
