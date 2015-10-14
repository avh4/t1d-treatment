module Glucose.SimulationTest where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Glucose.Simulation as Simulation


getBg (_,_,x) = x

all : Test
all =
  suite "Glucose.Simulation"
    [
      let
          params = Simulation.Parameters 30 14 1.000
          events = Simulation.eventsEmpty
          basals = 1.000
      in
          Simulation.calculate params basals events (Simulation.steadyIob basals,0,120)
      |> getBg
      |> assertEqual 120
      |> test "matched basal rate keeps bg steady",

      let
          params = Simulation.Parameters 30 14 0.000
          events =
              Simulation.eventsEmpty
              |> Simulation.eventsAddBolus 0 1.0
          basals = 0.000
      in
          Simulation.trace params basals events (Simulation.steadyIob basals,0,120)
      |> List.reverse
      |> List.head
      |> Maybe.map (getBg >> round)
      |> assertEqual (Just 90)
      |> test "bolus matches correction factor",

      let
          params = Simulation.Parameters 30 14 0.000
          events =
              Simulation.eventsEmpty
              |> Simulation.eventsAddFood 0 14
          basals = 0.000
      in
          Simulation.trace params basals events (Simulation.steadyIob basals,0,120)
      |> List.reverse
      |> List.head
      |> Maybe.map (getBg >> round)
      |> assertEqual (Just 150)
      |> test "food matches carb ratio"
    ]
