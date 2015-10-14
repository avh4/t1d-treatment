module Bayes
    ( DiscreteDistribution
    , uniformPrior1, uniformPrior2, uniformPrior3
    , marginal
    , normalize, map
    , update, updateAll
    , get
    ) where

import Dict exposing (Dict)


type alias Probability = Float

type alias DiscreteDistribution m =
    Dict m Probability


uniformPrior1 : List comparable -> DiscreteDistribution comparable
uniformPrior1 values =
    List.foldl (\a -> Dict.insert a 1) Dict.empty values
    |> normalize


uniformPrior2 :
    (comparable' -> b -> comparable)
    -> List comparable' -> List b
    -> DiscreteDistribution comparable
uniformPrior2 fn v1s bs =
    uniformPrior1 v1s
    |> multiply fn bs


uniformPrior3 :
    (comparable' -> comparable'' -> c -> comparable)
    -> List comparable' -> List comparable'' -> List c
    -> DiscreteDistribution comparable
uniformPrior3 fn v1s bs cs =
    uniformPrior1 v1s
    |> multiply (\x y -> (x,y)) bs
    |> multiply (\(x,y) z -> fn x y z) cs


multiply : (comparable -> b -> comparable') -> List b -> DiscreteDistribution comparable -> DiscreteDistribution comparable'
multiply merge vs dist =
    List.foldl (\v d ->
        Dict.foldl (\v0 p ->
            Dict.insert (merge v0 v) p) d dist)
        Dict.empty vs -- TODO: should update like marginal
    |> normalize


marginal : (comparable -> comparable') -> DiscreteDistribution comparable -> DiscreteDistribution comparable'
marginal fn =
    Dict.foldl (\m p -> Dict.update (fn m) (Maybe.withDefault 0 >> (+) p >> Just)) Dict.empty


normalize : DiscreteDistribution comparable -> DiscreteDistribution comparable
normalize model =
    let
        total = Dict.foldl (\_ p -> (+) p) 0 model
    in
        Dict.map (\_ v -> v/total) model


map : (comparable -> comparable') -> DiscreteDistribution comparable -> DiscreteDistribution comparable'
map =
    marginal


update :
    (o -> comparable -> Probability)
    -> o
    -> DiscreteDistribution comparable
    -> DiscreteDistribution comparable
update likelihood obs model =
    let
        up m p = p * (likelihood obs m)
    in
        model
        |> Dict.map up
        |> normalize


updateAll :
    (o -> comparable -> Probability)
    -> List o
    -> DiscreteDistribution comparable
    -> DiscreteDistribution comparable
updateAll likelihood observations modelDistribution =
    List.foldl (update likelihood) modelDistribution observations


get : comparable -> DiscreteDistribution comparable -> Probability
get m dist =
    Dict.get m dist
    |> Maybe.withDefault 0
