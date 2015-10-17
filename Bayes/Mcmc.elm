module Bayes.Mcmc (sample) where

import Bayes exposing (DiscreteDistribution)
import Random


step :
    (o -> m -> Float)
    -> (m -> Random.Seed -> (m,Random.Seed))
    -> o
    -> (m, Float, List m, Random.Seed)
    -> (m, Float, List m, Random.Seed)
step likelihood next obs (model,p,samples,seed) =
    let
        (model',seed') = next model seed
        p' = likelihood obs model'
        ratio = p' / p
        (r,seed'') = Random.generate (Random.float 0 1) seed'
    in
        if r < ratio then
            (model', p', model'::samples, seed'')
        else
            (model, p, samples, seed'')


sample :
    (o -> m -> Float)
    -> (m -> Random.Seed -> (m,Random.Seed))
    -> o -> Int -> m -> Random.Seed
    -> (List m, Random.Seed)
sample likelihood next obs iterations model0 seed =
    let
        p0 = likelihood obs model0
        init = (model0, p0, [model0], seed)
    in
        List.foldl (\_ -> step likelihood next obs) init [1..iterations]
        |> (\(_,_,samples,seed') -> (samples,seed'))
