module Stats
    ( normal
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
