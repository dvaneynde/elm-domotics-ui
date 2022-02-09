module Main exposing (..)

import Html exposing (text)
import Dict


main =
    text ("TEST: " ++ toString (isOpen (Dict.fromList [ ( "Screen", True ), ( "Buiten", False ) ]) "Buiten"))


isOpen : Dict.Dict String Bool -> String -> ( Bool, Dict.Dict String Bool )
isOpen blocks blockName =
    let
        enabled =
            Maybe.withDefault True (Dict.get blockName blocks)

        blocks2 =
            Dict.insert blockName enabled blocks
    in
        ( enabled, blocks2 )
