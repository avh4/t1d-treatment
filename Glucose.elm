module Glucose where

import Html exposing (Html)
import Graph
import Stats exposing (normal)
import Bayes exposing (DiscreteDistribution)
import StartApp
import Effects exposing (Effects)
import MatrixTable
import Dict


type alias BloodGlucoseReading = Float
type alias CorrectionFactor = Float
type alias CarbRatio = Float
type alias InsulinDose = Float
type alias FoodDose =
    { carbs : Float }

-- step :
--   (IobUnit, CobUnit, Bg)
--   -> Carbs
--   -> Bolus
--   -> Basal
--   -> CarbRatio
--   -> CorrectionFactor
--   -> Basal
--   -> (IobUnit, CobUnit, Bg)
-- step (iob0,cob0,bg0) food bolus basal kCarb kCorr qBasal =
--   let iob = iob0 + bolus + basal/60
--       cob = cob0 + food
--       di = (1-0.97)*iob
--       dc = (1-0.97)*cob
--       bg = bg0 - (di-q_basal/60)*kCorr + dc*k_carb
--   in
--       (iob-di, cob-dc, bg)


type alias Observation =
    { bg0 : BloodGlucoseReading
    , bg1 : BloodGlucoseReading
    , bolus : InsulinDose
    , food : FoodDose
    }


type alias Model = (CorrectionFactor, CarbRatio, InsulinDose)
    -- { correctionFactor : CorrectionFactor
    -- , carbRatio : CarbRatio
    -- , requiredBasal : InsulinDose
    -- }
model = (,,)


likelihood : Observation -> Model -> Float
likelihood obs model =
    let
        (correctionFactor, carbRatio, _) = model
    in
        if | carbRatio <= 0 -> 0
           | correctionFactor <= 0 -> 0
           | otherwise ->
              let carbFactor = carbRatio / correctionFactor
                  expectedBg1 =
                    obs.bg0
                    + obs.food.carbs / carbFactor
                    - (obs.bolus) * correctionFactor
                  bgSigma = 20
              in
                  normal.pdf expectedBg1 bgSigma obs.bg1


prior : DiscreteDistribution Model
prior =
    let
        corrs = [0..40]
        carbs = [0..30]
        basals = [0..20]
    in
        Bayes.uniformPrior3 model corrs carbs basals


-- MAIN


result' : Bayes.DiscreteDistribution Model
result' =
  prior
  |> flip (List.foldl (Bayes.update likelihood))
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

marg1 : Graph.DenseDataset
marg1 =
    result'
    |> Bayes.marginal (\(a,b,c) -> a)
    |> Dict.toList
    |> Graph.matrixDataset "Marginal Correction Factor"
        ("Correction Factor", fst)
        ("_", always 0)
        ("Proabability", snd)
        0


marg2 : Graph.DenseDataset
marg2 =
    result'
    |> Bayes.marginal (\(a,b,c) -> b)
    |> Dict.toList
    |> Graph.matrixDataset "Marginal Carb Ratio"
        ("Carb Ratio", fst)
        ("_", always 0)
        ("Proabability", snd)
        0


marg3 : Graph.DenseDataset
marg3 =
    result'
    |> Bayes.marginal (\(a,b,c) -> c)
    |> Dict.toList
    |> Graph.matrixDataset "Marginal Basal"
        ("Basal", fst)
        ("_", always 0)
        ("Proabability", snd)
        0


type alias MainModel = Maybe Float
type alias Action = MatrixTable.Action Float


view : Signal.Address Action -> MainModel -> Html
view address model =
    Html.div []
        [ Graph.matrix address result
        , Html.text <| toString model
        , Graph.matrix address marg1
        , Graph.matrix address marg2
        , Graph.matrix address marg3
        ]


update : Action -> MainModel -> (MainModel, Effects Action)
update action model =
    case action of
        MatrixTable.Hover v ->
            (Just v, Effects.none)


app : StartApp.App MainModel
app =
    StartApp.start
        { init = (Nothing, Effects.none)
        , update = update
        , view = view
        , inputs = []
        }


main : Signal Html
main = app.html
