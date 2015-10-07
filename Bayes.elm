module Bayes
    ( ModelSpace
    , normalize
    , update
    ) where


type alias ModelSpace m = List (List (m,Float))


normalize : ModelSpace m -> ModelSpace m
normalize model =
    let total = List.sum (List.concatMap (List.map snd) model)
        div (m,p) = (m,p/total)
    in
      List.map (List.map div) model


update : (o -> m -> Float) -> o -> ModelSpace m -> ModelSpace m
update likelihood obs model =
    let
        up (m,p) = (m, p * (likelihood obs m))
    in
        model
        |> List.map (List.map up)
        |> normalize
