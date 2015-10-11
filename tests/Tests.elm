module Tests where

import ElmTest.Test exposing (..)

import BayesTest
import StatsTest


all : Test
all =
  suite "all"
    [
      BayesTest.all,
      StatsTest.all
    ]
