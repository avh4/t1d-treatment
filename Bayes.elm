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
    (a -> b -> comparable)
    -> List a -> List b
    -> DiscreteDistribution comparable
uniformPrior2 fn v1s bs =
    List.foldl (\a d ->
        List.foldl (\b ->
            Dict.insert (fn a b) 1) d bs) Dict.empty v1s
    |> normalize


uniformPrior3 :
    (a -> b -> c -> comparable)
    -> List a -> List b -> List c
    -> DiscreteDistribution comparable
uniformPrior3 fn v1s bs cs =
    List.foldl (\a d ->
        List.foldl (\b d ->
            List.foldl (\c ->
                Dict.insert (fn a b c) 1) d cs) d bs) Dict.empty v1s
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
