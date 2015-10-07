module Viz (showMatrix) where


import Html exposing (Html)
import Html.Attributes as Html
import Table


color : Float -> Float -> Float -> Html
color min max x =
    let
        p = (x-min) / (max-min)
        c =
          if | p <=  1/6 -> "#dae8f5"
             | p <=  2/6 -> "#bad6ea"
             | p <=  3/6 -> "#88bedc"
             | p <=  4/6 -> "#539dcc"
             | p <=  5/6 -> "#297ab9"
             | otherwise -> "#09559f"
    in
      Html.div [ Html.style [("background", c), ("width", "5px"), ("height", "5px")]] []


showMatrix : List (List (m,Float)) -> Html
showMatrix model =
  let
      min = List.concat model |> List.map snd |> List.minimum |> Maybe.withDefault 0
      max = List.concat model |> List.map snd |> List.maximum |> Maybe.withDefault 1
  in
      Table.matrix (snd >> color min max) model
