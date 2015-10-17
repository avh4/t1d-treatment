module Stats
    ( normal
    , stddev
    , bins
    ) where


import Dict


type alias ContinuousDistribution =
    { pdf : Float -> Float
    }


normalPdf : Float -> Float -> Float -> Float
normalPdf mu sigma x =
  let exp = -((x - mu)^2) / (2*(sigma^2))
      k = 1 / (sigma * (sqrt (2*pi)))
  in
      k * (e^exp)


normal : Float -> Float -> ContinuousDistribution
normal mu sigma =
  { pdf = normalPdf mu sigma }


stddev : List Float -> Float
stddev xs =
    -- https://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods
    let
        step x (a,q,k) =
            let
                k' = k + 1
                a' = a + (x - a) / k'
                q' = q + (x - a) * (x - a')
            in
                (a',q',k')
    in
        List.foldl step (0,0,0) xs
        |> (\(_,q,n) -> q/(n-1))
        |> sqrt


bins : List Float -> List (Float, Float)
bins data =
    let
        n = List.length data
        sigma = stddev data
        binSize = 3.5 * sigma / ((toFloat n)^(1.0/3))
    in
        bins' binSize data


bins' : Float -> List (Float) -> List (Float, Float)
bins' size data =
    let
        min = List.minimum data |> Maybe.withDefault 0
        max = List.maximum data |> Maybe.withDefault (min+1)
        step list dict =
            case list of
                [] ->
                    dict
                (next::rest) ->
                    let
                        index = floor <| (next-min) / size
                        inc = (+) 1
                        d' = Dict.update index (Maybe.withDefault 0 >> inc >> Just) dict
                    in
                        step rest d'
    in
        step data Dict.empty
        |> Dict.toList
        |> List.map (\(x,y) -> ((toFloat x)*size+min,y))
