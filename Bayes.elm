module Bayes
    ( DiscreteDistribution
    , normalize, map
    , update
    ) where


type alias DiscreteDistribution m = List (m,Float)


normalize : DiscreteDistribution m -> DiscreteDistribution m
normalize model =
    let total = model |> List.map snd |> List.sum
        div (m,p) = (m,p/total)
    in
        List.map div model


map : (a -> b) -> DiscreteDistribution a -> DiscreteDistribution b
map f =
    List.map (\(a,p) -> (f a, p))


update : (o -> m -> Float) -> o -> DiscreteDistribution m -> DiscreteDistribution m
update likelihood obs model =
    let
        up (m,p) = (m, p * (likelihood obs m))
    in
        model
        |> List.map up
        |> normalize
