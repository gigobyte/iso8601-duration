module Iso8601.Duration exposing (Duration, fromString, parseDateElements, parseTimeElements)

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


type WeekElement
    = Weeks Float


type DateElement
    = Years Float
    | Months Float
    | Days Float


type TimeElement
    = Hours Float
    | Minutes Float
    | Seconds Float


fromString : String -> Maybe Duration
fromString duration =
    if not (String.startsWith "P" duration) then
        Nothing

    else if String.contains "W" duration then
        -- PnW
        case Parser.run weekParser duration of
            Ok (Weeks weeks) ->
                Just { initialDuration | days = weeks * 7 }

            Err _ ->
                Nothing

    else
        -- PnYnMnDTnHnMnS
        case String.split "T" (String.replace "," "." duration) of
            -- At least one element must be present, thus "P" is not a valid representation for a duration of 0 seconds
            [ "P", "" ] ->
                Nothing

            [ "P" ] ->
                Nothing

            [ date, time ] ->
                let
                    dateElements =
                        parseDateElements date

                    timeElements =
                        parseTimeElements time
                in
                case ( dateElements, timeElements ) of
                    ( Just [], Just [] ) ->
                        Nothing

                    _ ->
                        dateElements
                            |> Maybe.map (addDateElementsToDuration initialDuration)
                            |> (\intermediateDuration -> Maybe.map2 addTimeElementsToDuration intermediateDuration timeElements)

            [ date ] ->
                let
                    dateElements =
                        parseDateElements date
                in
                case dateElements of
                    Just [] ->
                        Nothing

                    _ ->
                        dateElements
                            |> Maybe.map (addDateElementsToDuration initialDuration)

            _ ->
                Nothing


parseDateElements : String -> Maybe (List DateElement)
parseDateElements date =
    Result.toMaybe (Parser.run dateElementsParser date)
        |> Maybe.andThen
            (\elements ->
                if isOutOfOrder isValidDateElementSequence elements then
                    Nothing

                else
                    Just elements
            )


parseTimeElements : String -> Maybe (List TimeElement)
parseTimeElements date =
    Result.toMaybe (Parser.run timeElementsParser date)
        |> Maybe.andThen
            (\elements ->
                if isOutOfOrder isValidTimeElementSequence elements then
                    Nothing

                else
                    Just elements
            )


addTimeElementsToDuration : Duration -> List TimeElement -> Duration
addTimeElementsToDuration =
    let
        addTimeElementToDuration : TimeElement -> Duration -> Duration
        addTimeElementToDuration element intermediateDuration =
            case element of
                Hours h ->
                    { intermediateDuration | hours = h }

                Minutes m ->
                    { intermediateDuration | minutes = m }

                Seconds s ->
                    { intermediateDuration | seconds = s }
    in
    List.foldr addTimeElementToDuration


addDateElementsToDuration : Duration -> List DateElement -> Duration
addDateElementsToDuration =
    let
        addDateElementToDuration : DateElement -> Duration -> Duration
        addDateElementToDuration element intermediateDuration =
            case element of
                Years y ->
                    { intermediateDuration | years = y }

                Months m ->
                    { intermediateDuration | months = m }

                Days d ->
                    { intermediateDuration | days = d }
    in
    List.foldr addDateElementToDuration



-- Parsers


weekParser : Parser WeekElement
weekParser =
    succeed Weeks
        |. symbol "P"
        |= float
        |. symbol "W"
        |. end


dateElementParser : List DateElement -> Parser (Step (List DateElement) (List DateElement))
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


dateElementsParser : Parser (List DateElement)
dateElementsParser =
    succeed identity
        |. symbol "P"
        |= loop [] dateElementParser


timeElementParser : List TimeElement -> Parser (Step (List TimeElement) (List TimeElement))
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


timeElementsParser : Parser (List TimeElement)
timeElementsParser =
    loop [] timeElementParser


isValidTimeElementSequence : TimeElement -> TimeElement -> Bool
isValidTimeElementSequence a b =
    case ( a, b ) of
        ( Minutes _, Hours _ ) ->
            True

        ( Seconds _, Minutes _ ) ->
            True

        ( Seconds _, Hours _ ) ->
            True

        _ ->
            False


isValidDateElementSequence : DateElement -> DateElement -> Bool
isValidDateElementSequence a b =
    case ( a, b ) of
        ( Months _, Years _ ) ->
            True

        ( Days _, Months _ ) ->
            True

        ( Days _, Years _ ) ->
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
