module Duration exposing (suite)

import Expect exposing (..)
import Iso8601.Duration exposing (fromString)
import Test exposing (..)


suite : Test
suite =
    describe "Duration parsing"
        [ test "P23DT23H" <|
            \_ ->
                "P23DT23H"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0
                            , months = 0
                            , days = 23
                            , hours = 23
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "P3Y6M4DT12H30M5S" <|
            \_ ->
                "P3Y6M4DT12H30M5S"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 3
                            , months = 6
                            , days = 4
                            , hours = 12
                            , minutes = 30
                            , seconds = 5
                            }
                        )
        , test "P12W" <|
            \_ ->
                "P12W"
                    |> fromString
                    |> Expect.equal
                        (Just
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
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0.5
                            , months = 0
                            , days = 0
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "P0.5Y0.5M" <|
            \_ ->
                "P0.5Y0.5M"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0.5
                            , months = 0.5
                            , days = 0
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "P0,5Y" <|
            \_ ->
                "P0,5Y"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0.5
                            , months = 0
                            , days = 0
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "PT36H" <|
            \_ ->
                "PT36H"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0
                            , months = 0
                            , days = 0
                            , hours = 36
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "P4Y" <|
            \_ ->
                "P4Y"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 4
                            , months = 0
                            , days = 0
                            , hours = 0
                            , minutes = 0
                            , seconds = 0
                            }
                        )
        , test "PT1M" <|
            \_ ->
                "PT1M"
                    |> fromString
                    |> Expect.equal
                        (Just
                            { years = 0
                            , months = 0
                            , days = 0
                            , hours = 0
                            , minutes = 1
                            , seconds = 0
                            }
                        )
        , test "PT" <|
            \_ ->
                "PT"
                    |> fromString
                    |> Expect.equal Nothing
        , test "PD" <|
            \_ ->
                "PD"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P1111111111 " <|
            \_ ->
                "P1111111111 "
                    |> fromString
                    |> Expect.equal Nothing
        , test "P" <|
            \_ ->
                "P"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P12WT12H30M5S" <|
            \_ ->
                "P12WT12H30M5S"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P0.5S0.5M" <|
            \_ ->
                "P0.5S0.5M"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P0.5A" <|
            \_ ->
                "P0.5A"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P5ST" <|
            \_ ->
                "P5ST"
                    |> fromString
                    |> Expect.equal Nothing
        , test "P5M10YT" <|
            \_ ->
                "P5ST"
                    |> fromString
                    |> Expect.equal Nothing
        ]
