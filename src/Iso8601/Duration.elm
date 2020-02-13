module Iso8601.Duration exposing (Duration)

import Parser exposing ((|.), (|=), Parser, backtrackable, float, oneOf, succeed, symbol)


type alias Duration =
    { years : Float
    , months : Float
    , days : Float
    , hours : Float
    , minutes : Float
    , seconds : Float
    }


type alias DurationComponents =
    { period : String
    , time : String
    }


componentsParser : Parser DurationComponents
componentsParser =
    oneOf
        []
