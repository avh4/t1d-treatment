module Tests where

import ElmTest.Test exposing (..)

import BayesTest
import StatsTest
import Glucose.SimulationTest


all : Test
all =
  suite "all"
    [
      BayesTest.all,
      StatsTest.all,
      Glucose.SimulationTest.all
    ]
