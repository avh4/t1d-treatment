module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import StatsTest


all : Test
all =
    suite "elm-stats"
        [ StatsTest.all
        ]
