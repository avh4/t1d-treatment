module Glucose where

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


result' : Bayes.DiscreteDistribution Model.Parameters
result' =
  Model.uniformPrior
  |> Bayes.updateAll Model.likelihood
      [ { bg0 = 276, bg1 =  61, bolus = 11, food = {carbs =  30} }
      , { bg0 = 129, bg1 = 175, bolus =  3, food = {carbs =  40} }
      , { bg0 =  69, bg1 = 103, bolus =  0, food = {carbs =  30} }
      , { bg0 = 171, bg1 =  69, bolus = 12, food = {carbs = 120} }
      ]

result : Graph.DenseDataset
result =
    result'
    |> Dict.toList
    |> Graph.matrixDataset "Model Distribution"
        ("Correction Factor", fst >> (\(x,_,_) -> x))
        ("Carb Ratio", fst >>  (\(_,x,_) -> x))
        ("Proabability", snd)
        0


marg1 : List (Float,Float)
marg1 =
    result'
    |> Bayes.marginal (\(a,b,c) -> a)
    |> Dict.toList


marg2 : List (Float,Float)
marg2 =
    result'
    |> Bayes.marginal (\(a,b,c) -> b)
    |> Dict.toList


marg3 : List (Float,Float)
marg3 =
    result'
    |> Bayes.marginal (\(a,b,c) -> c)
    |> Dict.toList


type alias ViewModel = Maybe Float
type alias Action = MatrixTable.Action Float


view : Signal.Address Action -> ViewModel -> Html
view address model =
    Html.div []
        [ Graph.matrix address result
        , Html.text <| toString model
        , Html.h3 [] [ Html.text "Correction Factor"]
        , Graph.histplot (300,200) marg1
        , Html.h3 [] [ Html.text "Carb Ratio"]
        , Graph.histplot (300,200) marg2
        , Html.h3 [] [ Html.text "Basal"]
        , Graph.histplot (300,200) marg3
        ]


update : Action -> ViewModel -> (ViewModel, Effects Action)
update action model =
    case action of
        MatrixTable.Hover v ->
            (Just v, Effects.none)


app : StartApp.App ViewModel
app =
    StartApp.start
        { init = (Nothing, Effects.none)
        , update = update
        , view = view
        , inputs = []
        }


main : Signal Html
main = app.html
