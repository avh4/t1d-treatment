module Tests where

import ElmTest.Test exposing (..)

import BayesTest


all : Test
all =
  suite "all"
    [
      BayesTest.all
    ]
