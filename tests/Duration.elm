module Duration exposing (suite)

import Expect exposing (..)
import Iso8601.Duration exposing (duration)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Duration parsing"
        [ test "P3Y6M4DT12H30M5S" <|
            \_ ->
                "P3Y6M4DT12H30M5S"
                    |> Parser.run duration
                    |> Expect.equal
                        (Ok
                            { years = 3
                            , months = 6
                            , days = 4
                            , hours = 12
                            , minutes = 30
                            , seconds = 5
                            }
                        )
        , test "P3Y6M2WT12H30M5S" <|
            \_ ->
                "P3Y6M2WT12H30M5S"
                    |> Parser.run duration
                    |> Expect.equal
                        (Ok
                            { years = 3
                            , months = 6
                            , days = 14
                            , hours = 12
                            , minutes = 30
                            , seconds = 5
                            }
                        )
        , test "P12W" <|
            \_ ->
                "P12W"
                    |> Parser.run duration
                    |> Expect.equal
                        (Ok
                            { years = 0
                            , months = 0
                            , days = 84
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "P0.5Y" <|
            \_ ->
                "P0.5Y"
                    |> Parser.run duration
                    |> Expect.equal
                        (Ok
                            { years = 0.5
                            , months = 0
                            , days = 0
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        ]
