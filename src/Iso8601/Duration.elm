module Iso8601.Duration exposing (Duration, fromString, toString)

{-| Convert ISO-8601 duration strings to a Duration value and vice versa.

@docs Duration, fromString, toString

-}

import Parser exposing ((|.), (|=), Parser, Step(..), end, float, loop, oneOf, problem, succeed, symbol)


{-| -}
type alias Duration =
    { years : Float
    , months : Float
    , days : Float
    , hours : Float
    , minutes : Float
    , seconds : Float
    }


initialDuration : Duration
initialDuration =
    { years = 0
    , months = 0
    , days = 0
    , hours = 0
    , minutes = 0
    , seconds = 0
    }


type Element
    = Years Float
    | Months Float
    | Days Float
    | Hours Float
    | Minutes Float
    | Seconds Float


{-| Convert ISO-8601 duration strings to a Duration value.

In case a week duration is given only the `days` property will be populated with the number of weeks \* 7.

-}
fromString : String -> Maybe Duration
fromString duration =
    if not (String.startsWith "P" duration) then
        Nothing

    else if String.contains "W" duration then
        -- PnW
        Parser.run weekParser duration
            |> Result.map (\weeks -> { initialDuration | days = weeks * 7 })
            |> Result.toMaybe

    else
        -- PnYnMnDTnHnMnS
        case duration |> String.replace "," "." |> String.split "T" of
            -- At least one element must be present, thus "P" is not a valid representation for a duration of 0 seconds
            [ "P" ] ->
                Nothing

            -- T specified but not time components
            [ date, "" ] ->
                Nothing

            [ date, time ] ->
                case ( parseElements dateElementsParser date, parseElements timeElementsParser time ) of
                    ( Just [], Just [] ) ->
                        Nothing

                    ( dateElements, timeElements ) ->
                        dateElements
                            |> Maybe.map (addElementsToDuration initialDuration)
                            |> (\intermediateDuration -> Maybe.map2 addElementsToDuration intermediateDuration timeElements)

            [ date ] ->
                case parseElements dateElementsParser date of
                    Just [] ->
                        Nothing

                    dateElements ->
                        Maybe.map (addElementsToDuration initialDuration) dateElements

            _ ->
                Nothing


{-| Convert a Duration value to a ISO-8601 duration string.

Week durations are not supported, even values with only days will still be serialized as `PnD`.

-}
toString : Duration -> String
toString duration =
    let
        timeElementsInDuration =
            duration.hours > 0 || duration.minutes > 0 || duration.seconds > 0

        elementToString : Float -> String -> String
        elementToString value element =
            if value == 0 then
                ""

            else
                String.fromFloat value ++ element
    in
    "P"
        ++ elementToString duration.years "Y"
        ++ elementToString duration.months "M"
        ++ elementToString duration.days "D"
        ++ (if timeElementsInDuration then
                "T"
                    ++ elementToString duration.hours "H"
                    ++ elementToString duration.minutes "M"
                    ++ elementToString duration.seconds "S"

            else
                ""
           )


parseElements : Parser (List Element) -> String -> Maybe (List Element)
parseElements elementParser date =
    Result.toMaybe (Parser.run elementParser date)
        |> Maybe.andThen
            (\elements ->
                if isOutOfOrder elements then
                    Nothing

                else
                    Just elements
            )


addElementsToDuration : Duration -> List Element -> Duration
addElementsToDuration =
    let
        addElementToDuration : Element -> Duration -> Duration
        addElementToDuration element intermediateDuration =
            case element of
                Years y ->
                    { intermediateDuration | years = y }

                Months m ->
                    { intermediateDuration | months = m }

                Days d ->
                    { intermediateDuration | days = d }

                Hours h ->
                    { intermediateDuration | hours = h }

                Minutes m ->
                    { intermediateDuration | minutes = m }

                Seconds s ->
                    { intermediateDuration | seconds = s }
    in
    List.foldr addElementToDuration



-- Parsers


weekParser : Parser Float
weekParser =
    succeed identity
        |. symbol "P"
        |= float
        |. symbol "W"
        |. end


dateElementParser : List Element -> Parser (Step (List Element) (List Element))
dateElementParser elements =
    oneOf
        [ succeed (\a b -> Loop (b a :: elements))
            |= float
            |= oneOf
                [ succeed Years |. symbol "Y"
                , succeed Months |. symbol "M"
                , succeed Days |. symbol "D"
                ]
        , succeed (Done elements)
        ]


dateElementsParser : Parser (List Element)
dateElementsParser =
    succeed identity
        |. symbol "P"
        |= loop [] dateElementParser


timeElementParser : List Element -> Parser (Step (List Element) (List Element))
timeElementParser elements =
    oneOf
        [ succeed (\a b -> Loop (b a :: elements))
            |= float
            |= oneOf
                [ succeed Hours |. symbol "H"
                , succeed Minutes |. symbol "M"
                , succeed Seconds |. symbol "S"
                ]
        , succeed (Done elements)
        ]


timeElementsParser : Parser (List Element)
timeElementsParser =
    loop [] timeElementParser


isValidElementSequence : Element -> Element -> Bool
isValidElementSequence a b =
    case ( a, b ) of
        ( Months _, Years _ ) ->
            True

        ( Days _, Months _ ) ->
            True

        ( Days _, Years _ ) ->
            True

        ( Minutes _, Hours _ ) ->
            True

        ( Seconds _, Minutes _ ) ->
            True

        ( Seconds _, Hours _ ) ->
            True

        _ ->
            False


isOutOfOrder : List Element -> Bool
isOutOfOrder elements =
    case elements of
        x :: y :: xs ->
            if not (isValidElementSequence x y) then
                True

            else
                isOutOfOrder (y :: xs)

        _ ->
            False
