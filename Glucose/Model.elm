module Glucose.Model where

import Html exposing (Html)
import Graph
import Stats exposing (normal)
import Bayes exposing (DiscreteDistribution)
import StartApp
import Effects exposing (Effects)
import MatrixTable
import Dict
import Array


type alias BloodGlucoseReading = Float
type alias CorrectionFactor = Float
type alias CarbRatio = Float
type alias InsulinDose = Float
type alias FoodDose =
    { carbs : Float }


type alias Observation =
    { bg0 : BloodGlucoseReading
    , bg1 : BloodGlucoseReading
    , bolus : InsulinDose
    , food : FoodDose
    }


type alias Parameters = (CorrectionFactor, CarbRatio, InsulinDose)
    -- { correctionFactor : CorrectionFactor
    -- , carbRatio : CarbRatio
    -- , requiredBasal : InsulinDose
    -- }
model = (,,)


likelihood : Observation -> Parameters -> Float
likelihood obs model =
    let
        (correctionFactor, carbRatio, _) = model
    in
        if carbRatio <= 0 then 0
        else if correctionFactor <= 0 then 0
        else
            let carbFactor = carbRatio / correctionFactor
                expectedBg1 =
                  obs.bg0
                  + obs.food.carbs / carbFactor
                  - (obs.bolus) * correctionFactor
                bgSigma = 20
            in
                (normal expectedBg1 bgSigma).pdf obs.bg1


uniformPrior : DiscreteDistribution Parameters
uniformPrior =
    let
        corrs = [0..40]
        carbs = [0..30]
        basals = [0..20]
    in
        Bayes.uniformPrior3 model corrs carbs basals
