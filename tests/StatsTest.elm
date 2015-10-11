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
      |> test "normal distribution probability density function"
    ]
