module Glucose where

import Html exposing (Html)
import Graph
import Stats exposing (normal)
import Bayes exposing (ModelSpace)


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


type alias Model =
    { correctionFactor : CorrectionFactor
    , carbRatio : CarbRatio
    }


likelihood : Observation -> Model -> Float
likelihood obs model =
    if | model.carbRatio <= 0 -> 0
       | model.correctionFactor <= 0 -> 0
       | otherwise ->
          let carbFactor = model.carbRatio / model.correctionFactor
              expectedBg1 =
                obs.bg0
                + obs.food.carbs / carbFactor
                - obs.bolus * model.correctionFactor
              bgSigma = 20
          in
              normal.pdf expectedBg1 bgSigma obs.bg1


initModelSpace : ModelSpace Model
initModelSpace =
    let
        corrs = [0..40]
        carbs = [0..30]
    in
        List.map (\a -> List.map (\b -> (Model a b, 1)) carbs) corrs
        |> Bayes.normalize


-- MAIN


main : Html
main =
    initModelSpace
    |> flip (List.foldl (Bayes.update likelihood))
        [ { bg0 = 276, bg1 =  61, bolus = 11, food = {carbs =  30} }
        , { bg0 = 129, bg1 = 175, bolus =  3, food = {carbs =  40} }
        , { bg0 =  69, bg1 = 103, bolus =  0, food = {carbs =  30} }
        , { bg0 = 171, bg1 =  69, bolus = 12, food = {carbs = 120} }
        ]
    |> List.map (List.map snd)
    |> Graph.matrix
