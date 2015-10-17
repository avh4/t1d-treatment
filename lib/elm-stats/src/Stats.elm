module Stats
    ( normal
    , stddev
    ) where


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
