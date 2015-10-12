module Glucose.Simulation where

import Dict exposing (Dict)


type alias BloodGlucoseReading = Float
type alias CorrectionFactor = Float
type alias CarbRatio = Float
type alias InsulinDose = Float
type alias FoodDose =
    { carbs : Float }
type alias IobUnit = Float
type alias CobUnit = Float
type alias UnitsPerHour = Float


type alias Parameters =
    { correctionFactor : CorrectionFactor
    , carbRatio : CarbRatio
    , requiredBasal : UnitsPerHour
    }


type alias TimeStep =
    { food : FoodDose
    , bolus : InsulinDose
    , basal : UnitsPerHour
    }

iobDecay = 0.97


steadyIob : UnitsPerHour -> IobUnit
steadyIob basal =
    basal * -(iobDecay/(iobDecay-1)) / 60


step :
    Parameters
    -> TimeStep
    -> (IobUnit, CobUnit, BloodGlucoseReading)
    -> (IobUnit, CobUnit, BloodGlucoseReading)
step params event (iob0,cob0,bg0) =
    let
        bolus = event.bolus
        food = event.food.carbs
        basal = event.basal

        kCarb = params.correctionFactor/params.carbRatio
        kCorr = params.correctionFactor
        qBasal = params.requiredBasal
        iob = iob0 + bolus + basal/60
        cob = cob0 + food
        di = (1-iobDecay)*iob
        dc = (1-0.97)*cob
        bg = bg0 - (di-qBasal/60)*kCorr + dc*kCarb
    in
        (iob-di, cob-dc, bg)


-- TODO: multiple events at same time
type Event
    = Food Float
    | Bolus Float


type alias Events =
    Dict Int Event


eventsEmpty : Events
eventsEmpty =
    Dict.empty


eventsAddFood : Int -> Float -> Events -> Events
eventsAddFood t carbs =
    Dict.insert t (Food carbs)


eventsAddBolus : Int -> Float -> Events -> Events
eventsAddBolus t ins =
    Dict.insert t (Bolus ins)


type alias DefaultBasals = Float


generate :
    Parameters
    -> DefaultBasals
    -> Events
    -> (IobUnit, CobUnit, BloodGlucoseReading)
    -> List (IobUnit, CobUnit, BloodGlucoseReading)
generate params basals events init =
    let
        event0 = TimeStep {carbs=0} 0 basals
        init' = (init,[],event0)
        step' t (last,acc,event) =
            let
                event' =
                    case Dict.get t events of
                        Nothing ->
                            TimeStep {carbs=0} 0 event.basal
                        Just (Food f) ->
                            TimeStep {carbs=f} 0 event.basal
                        Just (Bolus i) ->
                            TimeStep {carbs=0} i event.basal
            in
                ((step params event' last),last::acc,event')
    in
        List.foldl step' init' [0..180]
        |> (\(last,acc,_) -> last::acc)
        |> List.reverse
