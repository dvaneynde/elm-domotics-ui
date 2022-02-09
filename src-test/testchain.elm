module Main exposing (..)

import Html exposing (text)
import Json.Decode exposing (..)


{- Chains components of type a, that need a unique number when created. -}


chain : (Int -> a) -> ( List a, Int ) -> ( List a, Int )
chain a ( sofar, nr ) =
        ( List.append sofar [ (a nr) ], nr + 1 )


type alias Model =
    String


type alias HtmlMsg =
    String


widgetA : Int -> Model -> HtmlMsg
widgetA nr model =
    "Widget AAA [" ++ (toString nr) ++ "] onto model '" ++ model ++ "'"


widgetB : String -> Int -> Model -> HtmlMsg
widgetB attribute nr model =
    "widget BBB [" ++ (toString nr) ++ "] with attribute {" ++ attribute ++ "} onto model '" ++ model ++ "'"


view : Model -> HtmlMsg
view model =
    let
{-
        widgetList = [ widgetA 0, widgetB "inner" 1, widgetB "outer" 2 ]
-}
{--}
        ( widgetList, _ ) =
            -- chain widgetA (chain widgetB (chain widgetA ( [], 0)))
            ( [], 0 ) |> chain widgetA |> chain (widgetB "inner") |> chain (widgetB "outer")
--}
    in
        -- widgetList is list of functions Model -> Html, so apply model to them to generate Html Msg
        -- each time view is to be built
        toString (List.map (\widget -> widget model) widgetList)


main =
    text (view "model_185468434")
