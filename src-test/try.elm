module Main exposing (..)

import Html exposing (text)
import Time exposing (..)


main =
    --text (toString ([1..10]))
    --text (toString (List.foldl (::) [] (List.map tick [1..10])))
    --text (List.foldr stringConcat "" (List.map tick [1..10]))
    --text (toString (List.concat [innernumber, [4,5]]))
    --text (toString (Time.inSeconds 10256))
    text (toString ([ 1, 2 ] ++ [ 3, 4 ]))


innernumber : List Int
innernumber =
    [ 1, 2 ]


stringConcat : String -> String -> String
stringConcat s0 s1 =
    s0 ++ ", " ++ s1


tick : Int -> String
tick i =
    "tick" ++ (toString i)
