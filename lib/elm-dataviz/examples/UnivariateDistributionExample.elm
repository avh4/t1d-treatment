module UnivariateDistributionExample where

import Html
import Graph


histogram =
    [ (40, 1)
    , (50, 2)
    , (60, 5)
    , (70, 12)
    , (80, 10)
    , (90, 19)
    , (100, 27)
    , (110, 13)
    , (120, 7)
    , (130, 4)
    ]


observations =
    [ 1.5, 5, 7, 3, 20, 16, 7, 5.2, 7.01, 18 ]


main =
    Html.div []
        [ histogram |> Graph.histplot (300, 200)
        , observations |> Graph.distplot (300, 200)
        ]
