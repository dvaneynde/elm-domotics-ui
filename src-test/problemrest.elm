module Main exposing (..)

import Dict
import Html exposing (Html, button, div, text, span, input, label, br, meter)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, int, float, string, bool, map7, field)
import Json.Encode as Encode


main =
    Html.program { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }



-- MODEL


type alias StatusRecord =
    { name : String, kind : String, group : String, description : String, on : Bool, level : Int, status : String }


type alias Model =
    { statuses : List StatusRecord, errorMsg : String, test : String }


init : ( Model, Cmd Msg )
init =
    ( { statuses = [], errorMsg = "No worries...", test = "nothing tested" }, Cmd.none )


initialStatus : StatusRecord
initialStatus =
    { name = "", kind = "", group = "", description = "", on = False, level = 0, status = "" }



-- UPDATE


type Msg
    = GetStatus
    | GetStatusAsString
    | NewStatusViaRest (Result Http.Error (List StatusRecord))
    | NewStatusViaRestString (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStatus ->
            ( model, updateStatusViaRestCmd "LichtKeuken" "on" )

        GetStatusAsString ->
            ( model, updateStatusViaRestCmdString "LichtKeuken" "on" )

        NewStatusViaRest (Ok statuses_) ->
            ( {  model | statuses = statuses_, errorMsg = "OK" }, Cmd.none )

        NewStatusViaRest (Err message) ->
            ( { model | errorMsg = ("NewStatusViaRest: "++ (toString message)) }, Cmd.none )

        NewStatusViaRestString (Ok statuses_) ->
            ( {  model | test = statuses_, errorMsg = "OK" }, Cmd.none )

        NewStatusViaRestString (Err message) ->
            ( { model | errorMsg = ("NewStatusViaRestString: "++ (toString message)) }, Cmd.none )


decodeStatuses : String -> ( List StatusRecord, String )
decodeStatuses strJson =
    let
        result =
            decodeString statusesDecoder strJson
    in
        case result of
            Ok value ->
                ( value, "" )

            Err error ->
                ( [], error )


statusesDecoder : Decoder (List StatusRecord)
statusesDecoder =
    Decode.list statusDecoder


statusDecoder : Decoder StatusRecord
statusDecoder =
    map7 StatusRecord
        (field "name" string)
        (field "type" string)
        (field "description" string)
        (field "groupName" string)
        (field "on" bool)
        (field "level" int)
        (field "status" string)


updateStatusViaRestCmd : String -> String -> Cmd Msg
updateStatusViaRestCmd name value =
    let
        url = "http://localhost:8080/rest/act/" ++ name ++ "/" ++ value
        request = Http.get url statusesDecoder
    in
        --Task.perform RestError NewStatusViaRest ()
        Http.send NewStatusViaRest request

updateStatusViaRestCmdString : String -> String -> Cmd Msg
updateStatusViaRestCmdString name value =
    let
        url = "http://localhost:8080/rest/act/" ++ name ++ "/" ++ value
        request = Http.getString url
    in
        Http.send NewStatusViaRestString request



-- VIEW


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style [ ( "padding", "2rem" ), ( "background", "azure" ) ] ]
        [
            button [ onClick GetStatus ] [ text "Get Status" ],
            Html.br [] [],
            button [ onClick GetStatusAsString ] [ text "Get Status As String" ],
            Html.br [] [],
            text (toString model)
        ]
