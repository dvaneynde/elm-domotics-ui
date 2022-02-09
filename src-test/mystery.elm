module Main exposing (..)

import Html exposing (text)


main =
    -- dit werkt, dus functie met #2 return value: text (toString (storm "3 beaufort"))
    text (toString (Wind "Hoezee"))



-- dit werkt ook, Alarm heeft constructor voor elke mogelijkheid


type Alarm a
    = Wind a
    | Licht a
    | Onbekend



-- DIT IS HET: return value is Alarm String, zoals Http Msg
-- runtime gaat dan berichten genereren zoals hieronder 'Wind heel-hard'


storm : String -> Alarm String
storm a =
    Wind a


printIt : Alarm String -> String
printIt b =
    case b of
        Wind a ->
            "Storm " ++ a

        Licht a ->
            "Verblind " ++ a

        Onbekend ->
            "?"
