module GlucoseMcmc where

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
import Random
import Bayes.Mcmc as Mcmc


obs0 = { bg0 = 276, bg1 =  61, bolus = 11, food = {carbs =  30} }

obs =
      [ obs0
      , { bg0 = 129, bg1 = 175, bolus =  3, food = {carbs =  40} }
      , { bg0 =  69, bg1 = 103, bolus =  0, food = {carbs =  30} }
      , { bg0 = 171, bg1 =  69, bolus = 12, food = {carbs = 120} }
      ]

next model seed =
    let
        (da,seed') = Random.generate (Random.float -1 1) seed
        (db,seed'') = Random.generate (Random.float -1 1) seed'
        (dc,seed''') = Random.generate (Random.float -1 1) seed''
        (a,b,c) = model
        model' = (a+da,b+db,c+dc)
    in
        (model', seed''')


main =
    let
        seed0 = Random.initialSeed 42
        model0 = (,,) 1 1 1

        (result,seed1) = Mcmc.sample Model.likelihood next obs0 10000 model0 seed0
    in
        Html.div []
            [ Html.h2 [] [ Html.text "Correciton Factor" ]
            , result |> List.map (\(x,_,_) -> x) |> Graph.distplot (300,200)
            , Html.h2 [] [ Html.text "Carb Ratio" ]
            , result |> List.map (\(_,x,_) -> x) |> Graph.distplot (300,200)
            , Html.h2 [] [ Html.text "Required Basal" ]
            , result |> List.map (\(_,_,x) -> x) |> Graph.distplot (300,200)
            ]
