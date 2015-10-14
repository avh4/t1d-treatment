module SimulationDemo where

import Html exposing (Html)
import Graph
import Stats exposing (normal)
import Bayes exposing (DiscreteDistribution)
import StartApp
import Effects exposing (Effects)
import MatrixTable
import Dict
import Array
import Glucose.Model as Model
import Glucose.Simulation as Simulation


data : List (Int, (Float, Float, Float))
data =
    let
        params = Simulation.Parameters 30 14 1.000
        events =
          Simulation.eventsEmpty
          |> Simulation.eventsAddFood 10 14
          |> Simulation.eventsAddBolus 12 2
        basals = 1.000
    in
        Simulation.trace params basals events (Simulation.steadyIob basals,0,120) 180
        |> List.indexedMap (,)


iob =
    Graph.xyDataset "Insulin On Board"
        ("Time (seconds)", fst >> toFloat)
        ("Insulin On Board (U)", snd >> (\(x,_,_) -> x))
        data


cob =
    Graph.xyDataset "Carbs On Board"
        ("Time (seconds)", fst >> toFloat)
        ("Carbs On Board (U)", snd >> (\(_,x,_) -> x))
        data


bg =
    Graph.xyDataset "Blood Glucose"
        ("Time (seconds)", fst >> toFloat)
        ("Blood glucose (mg/dL)", snd >> (\(_,_,x) -> x))
        data


main = Graph.graph (700, 200)
  [ bg, iob, cob ]
