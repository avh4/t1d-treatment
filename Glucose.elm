module Glucose where

import Html exposing (Html)
import Viz
import Debug


type alias Probability = Float -- [0, 1]


normalPdf : Float -> Float -> Float -> Float
normalPdf mu sigma x =
  let exp = -((x - mu)^2) / (2*(sigma^2))
      k = 1 / (sigma * (sqrt (2*pi)))
  in
      k * (e^exp)


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

likelihood : Observation -> Model -> Probability
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
              normalPdf expectedBg1 bgSigma obs.bg1


type alias ModelSpace m = List (List (m,Probability))


normalize : ModelSpace m -> ModelSpace m
normalize model =
    let total = List.sum (List.concatMap (List.map snd) model)
        div (m,p) = (m,p/total)
    in
      List.map (List.map div) model


initModelSpace : ModelSpace Model
initModelSpace =
    let
        corrs = [0..40]
        carbs = [0..30]
    in
        List.map (\a -> List.map (\b -> (Model a b, 1)) carbs) corrs
        |> normalize


update : (o -> m -> Probability) -> o -> ModelSpace m -> ModelSpace m
update likelihood obs model =
    let
        up (m,p) = (m, p * (likelihood obs m))
    in
        model
        |> List.map (List.map up)
        |> normalize


-- MAIN


main : Html
main =
    initModelSpace
    |> flip (List.foldr (update likelihood))
        [ { bg0 = 276, bg1 =  61, bolus = 11, food = {carbs =  30} }
        , { bg0 = 129, bg1 = 175, bolus =  3, food = {carbs =  40} }
        , { bg0 =  69, bg1 = 103, bolus =  0, food = {carbs =  30} }
        , { bg0 = 171, bg1 =  69, bolus = 12, food = {carbs = 120} }
        ]
    |> Viz.showMatrix
