module Stats (normal) where


normalPdf : Float -> Float -> Float -> Float
normalPdf mu sigma x =
  let exp = -((x - mu)^2) / (2*(sigma^2))
      k = 1 / (sigma * (sqrt (2*pi)))
  in
      k * (e^exp)


normal : { pdf : Float -> Float -> Float -> Float }
normal =
  { pdf = normalPdf }
