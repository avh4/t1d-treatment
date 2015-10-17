module Tests where

import ElmTest.Test exposing (..)

import BayesTest
import Glucose.SimulationTest


all : Test
all =
  suite "all"
    [
      BayesTest.all,
      Glucose.SimulationTest.all
    ]
