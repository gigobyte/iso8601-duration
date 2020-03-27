module Iso8601.Duration exposing (Duration, fromString)

import Parser exposing ((|.), (|=), Parser, Step(..), end, float, loop, oneOf, problem, succeed, symbol)


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
        case String.split "T" (String.replace "," "." duration) of
            -- At least one element must be present, thus "P" is not a valid representation for a duration of 0 seconds
            [ "P", "" ] ->
                Nothing

            [ "P" ] ->
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


parseElements : Parser (List Element) -> String -> Maybe (List Element)
parseElements elementParser date =
    Result.toMaybe (Parser.run elementParser date)
        |> Maybe.andThen
            (\elements ->
                if isOutOfOrder isValidElementSequence elements then
                    Nothing

                else
                    Just elements
            )


addElementsToDuration : Duration -> List Element -> Duration
addElementsToDuration =
    let
        addDateElementToDuration : Element -> Duration -> Duration
        addDateElementToDuration element intermediateDuration =
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
    List.foldr addDateElementToDuration



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



-- List util


isOutOfOrder : (a -> a -> Bool) -> List a -> Bool
isOutOfOrder compare elements =
    let
        isOutOfOrderHelper : List a -> Bool
        isOutOfOrderHelper xs =
            case xs of
                x :: y :: others ->
                    if not (compare x y) then
                        True

                    else
                        isOutOfOrderHelper (y :: others)

                _ ->
                    False
    in
    isOutOfOrderHelper elements
