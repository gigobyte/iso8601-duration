module Iso8601.Duration exposing (Duration, duration)

import Parser exposing ((|.), (|=), Parser, backtrackable, float, oneOf, succeed, symbol)


type alias Duration =
    { years : Float
    , months : Float
    , days : Float
    , hours : Float
    , minutes : Float
    , seconds : Float
    }


duration : Parser Duration
duration =
    succeed Duration
        |. period
        |= oneOf [ backtrackable years, succeed 0 ]
        |= oneOf [ backtrackable months, succeed 0 ]
        |= oneOf [ backtrackable daysFromWeek, days ]
        |. oneOf [ timeDesignator, succeed () ]
        |= oneOf [ backtrackable hours, succeed 0 ]
        |= oneOf [ backtrackable minutes, succeed 0 ]
        |= oneOf [ backtrackable seconds, succeed 0 ]


period : Parser ()
period =
    symbol "P"


years : Parser Float
years =
    float |. symbol "Y"


months : Parser Float
months =
    float |. symbol "M"


daysFromWeek : Parser Float
daysFromWeek =
    float
        |. symbol "W"
        |> Parser.map ((*) 7)


days : Parser Float
days =
    float |. symbol "D"


timeDesignator : Parser ()
timeDesignator =
    symbol "T"


hours : Parser Float
hours =
    float |. symbol "H"


minutes : Parser Float
minutes =
    float |. symbol "M"


seconds : Parser Float
seconds =
    float |. symbol "S"
