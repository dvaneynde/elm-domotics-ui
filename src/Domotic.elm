port module Domotic exposing (..)

{- in Safari, Develop, "Disable Cross-Origin Restrictions".
   But when on same server no problem.
   https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Access-Control-Allow-Origin
-}

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, button, div, text, span, input, label, br, meter, hr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Url exposing (Url)
import Http
import Json.Decode as Decode exposing (Decoder, int, string, bool, field, oneOf, succeed)


-- Elm tells JS which WebSocket URL to connect to
port connectWebSocket : String -> Cmd msg

-- JS forwards incoming WebSocket messages to Elm
port newStatusViaWs : (String -> msg) -> Sub msg


----------------------------------------------------------
-- URL's and WebSocket address

{-
   Set to Nothing for production use, set to backend host and port if using a local web server.
   TODO make parameter of program, so that it is set to Nothing from index.html
-}
fixBackendHostPort : Maybe String
fixBackendHostPort =
    -- Just "192.168.0.10:80"
    -- Just "127.0.0.1:80"
    Nothing

{-
   Host and port to use for backend; taken from URL of this webapp, unless fixBackendHostPort is set.
   Note that "host" in javascript URL includes the port (if not 80 or 443) which is not according to
   standard: https://stackoverflow.com/questions/9260218/parts-of-a-url-host-port-path
-}


getBackendHostPort : Url -> String
getBackendHostPort url =
    case fixBackendHostPort of
        Just hostAndPort ->
            hostAndPort

        Nothing ->
            case url.port_ of
                Just p ->
                    url.host ++ ":" ++ String.fromInt p

                Nothing ->
                    url.host


urlUpdateUiBlocks : Model -> String
urlUpdateUiBlocks model =
    "http://" ++ model.hostAndPort ++ "/rest/actuators/"



----------------------------------------------------------
-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "Domotics", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = \_ -> NoOp
        }



----------------------------------------------------------
-- MODEL

-- Which groups are expanded or collapsed
type alias Group2ExpandedDict =
    Dict.Dict String Bool


type alias MeterAttributes =
    { min : Int, low : Int, high : Int, max : Int }


type ExtraStatus
    = None
    | OnOff Bool
    | OnOffLevel Bool Int
    | Level Int MeterAttributes
    | OnOffEco Bool Bool


type alias StatusRecord =
    { name : String, kind : String, groupName : String, groupSeq : Int, description : String, status : String, extra : ExtraStatus }


type alias Groups =
    Dict.Dict String (List StatusRecord)


type alias Model =
    { groups : Groups
    , group2Expanded : Group2ExpandedDict
    , errorMsg : Maybe String
    , testMsg : Maybe String
    , hostAndPort : String
    , navKey : Nav.Key
    }


initialStatus : StatusRecord
initialStatus =
    { name = "", kind = "", groupName = "", groupSeq = 0, description = "", status = "", extra = None }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let -- TODO refactor to not need to get host and port twice (also in update when URL changes), or remove LED
        hostAndPort =
            getBackendHostPort url
    in
    ( { groups = Dict.empty
      , group2Expanded = initGroups
      , errorMsg = Nothing
      , testMsg = Nothing
      , hostAndPort = hostAndPort
      , navKey = key
      }
    , Cmd.batch
        [ Http.get
            { url = "http://" ++ hostAndPort ++ "/rest/statuses/"
            , expect = Http.expectJson GotInitialStatus statusesDecoder
            }
        , connectWebSocket ("ws://" ++ hostAndPort ++ "/status/")
        ]
    )


initGroups : Group2ExpandedDict
initGroups =
    Dict.fromList
        [ ( "ScreensZ", False )
        , ( "ScreensW", False )
        , ( "Beneden", True )
        , ( "Nutsruimtes", True )
        , ( "Kinderen", True )
        , ( "Buiten", False )
        ]


statusByName : String -> Groups -> StatusRecord
statusByName name groups =
    let
        listOfRecords =
            List.foldr (++) [] (Dict.values groups)

        filteredList =
            List.filter (\rec -> rec.name == name) listOfRecords
    in
    Maybe.withDefault initialStatus (List.head filteredList)



----------------------------------------------------------
-- UPDATE


type Msg
    = NoOp
    | PutModelInTestAsString
    | ClearTestMessage
    | ClearErrorMessage
    | Clicked String
    | ClickedEco String
    | Checked String Bool
    | Down String
    | Up String
    | SliderMsg String String
    | ToggleShowBlock String
    | NewStatus String
    | GotInitialStatus (Result Http.Error (List StatusRecord))
    | PostUiUpdateResult (Result Http.Error ())
    | UrlChanged Url
    | CollapseAllGroups
    | OpenAllGroups


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PutModelInTestAsString ->
--            ( { model | testMsg = (toString { model | testMsg = "" }.groups) }, Cmd.none )
--            ( { model | testMsg = (toString (Dict.get "Beneden" { model | testMsg = "" }.groups)) }, Cmd.none )
            ( { model | testMsg = Just (Debug.toString model.groups) }, Cmd.none )

        ClearTestMessage ->
            ( { model | testMsg = Nothing }, Cmd.none )

        ClearErrorMessage ->
            ( { model | errorMsg = Nothing }, Cmd.none )

        Clicked what ->
            let
                extra =
                    (statusByName what model.groups).extra

                onOffText =
                    if isOn extra then
                        "off"
                    else
                        "on"
            in
            ( model, updateStatusViaRestCmd model what onOffText )

        ClickedEco what ->
            ( model, updateStatusViaRestCmd model what "ecoToggle" )

        Checked what value ->
            ( model
            , updateStatusViaRestCmd model
                what
                (if value then
                    "on"
                 else
                    "off"
                )
            )

        Down what ->
            ( model, updateStatusViaRestCmd model what "down" )

        Up what ->
            ( model, updateStatusViaRestCmd model what "up" )

        SliderMsg what level ->
            ( model, updateStatusViaRestCmd model what level )

        NewStatus strJson ->
            case Decode.decodeString statusesDecoder strJson of
                Ok newStatuses ->
                    ( { model | groups = createGroups newStatuses, errorMsg = Nothing }, Cmd.none )

                Err error ->
                    ( { model | errorMsg = Just (Decode.errorToString error) }, Cmd.none )

        ToggleShowBlock name ->
            ( { model | group2Expanded = toggleGroup2Open model.group2Expanded name }, Cmd.none )

        CollapseAllGroups ->
            ( { model | group2Expanded = Dict.map (\_ _ -> False) model.group2Expanded }, Cmd.none )

        OpenAllGroups ->
            ( { model | group2Expanded = Dict.map (\_ _ -> True) model.group2Expanded }, Cmd.none )

        GotInitialStatus (Ok statuses) ->
            ( { model | groups = createGroups statuses, errorMsg = Nothing }, Cmd.none )

        GotInitialStatus (Err error) ->
            ( { model | errorMsg = Just ("Failed to load status: " ++ httpErrorToString error) }, Cmd.none )

        PostUiUpdateResult (Ok ()) ->
            ( model, Cmd.none )

        PostUiUpdateResult (Err message) ->
            -- Gives an error 'BadPayload "Given an invalid JSON: JSON Parse error: Unexpected EOF',
            -- but not really an error. Probably because received body is empty (204 No Content)
            -- yet Elm still wants to parse it. Treat as non-fatal, log to console.
            ( Debug.log ("PostUiUpdateResult: " ++ Debug.toString message) model, Cmd.none )

        UrlChanged url ->
            ( { model | hostAndPort = getBackendHostPort url }, Cmd.none )


createGroups : List StatusRecord -> Groups
createGroups statuses =
    let
        -- Gather doet iets raars, lijst van lijst en in inner lijst komt het eerste record en dan de lijst van de rest. Slim eigenlijk.
        listOfGroups =
            gatherWith (\a b -> a.groupName == b.groupName) statuses

        withSortedLists =
            List.map (\( r, l ) -> ( r.groupName, List.sortBy .groupSeq (r :: l) )) listOfGroups
    in
    Dict.fromList withSortedLists


isGroupOpen : Group2ExpandedDict -> String -> Bool
isGroupOpen blocks blockName =
    Maybe.withDefault True (Dict.get blockName blocks)


toggleGroup2Open : Group2ExpandedDict -> String -> Group2ExpandedDict
toggleGroup2Open group2Expanded name =
    let
        func maybe =
            case maybe of
                Just b ->
                    Just (not b)

                Nothing ->
                    Just True
    in
    Dict.update name func group2Expanded



----------------------------------------------------------
-- DECODERS


statusesDecoder : Decoder (List StatusRecord)
statusesDecoder =
    Decode.list statusDecoder


statusDecoder : Decoder StatusRecord
statusDecoder =
    Decode.map7 StatusRecord
        (field "name" string)
        (field "type" string)
        (field "groupName" string)
        (field "groupSeq" int)
        (field "description" string)
        (field "status" string)
        (oneOf [ decoderExtraOnOffLevel, decoderExtraLevel, decoderExtraOnOffEco, decoderExtraOnOff, succeed None ])


decoderExtraOnOffLevel : Decoder ExtraStatus
decoderExtraOnOffLevel =
    Decode.map2 OnOffLevel (field "on" bool) (field "level" int)


decoderExtraLevel : Decoder ExtraStatus
decoderExtraLevel =
    Decode.map2 Level (field "level" int) decoderMeterAttributes


decoderMeterAttributes : Decoder MeterAttributes
decoderMeterAttributes =
    Decode.map4 MeterAttributes (field "min" int) (field "low" int) (field "high" int) (field "max" int)


decoderExtraOnOff : Decoder ExtraStatus
decoderExtraOnOff =
    Decode.map OnOff (field "on" bool)


decoderExtraOnOffEco : Decoder ExtraStatus
decoderExtraOnOffEco =
    Decode.map2 OnOffEco (field "on" bool) (field "eco" bool)



----------------------------------------------------------
-- HTTP


updateStatusViaRestCmd : Model -> String -> String -> Cmd Msg
updateStatusViaRestCmd model name value =
    Http.post
        { url = urlUpdateUiBlocks model ++ name ++ "/" ++ value
        , body = Http.emptyBody
        , expect = Http.expectWhatever PostUiUpdateResult
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad response body: " ++ body



----------------------------------------------------------
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    newStatusViaWs NewStatus



----------------------------------------------------------
-- VIEW
-- https://design.google.com/icons/


levelByName : String -> Groups -> Float
levelByName name groups =
    case (statusByName name groups).extra of
        OnOffLevel _ level ->
            toFloat level

        Level level _ ->
            toFloat level

        _ ->
            0.0


isOnByName : String -> Groups -> Bool
isOnByName name groups =
    isOn (statusByName name groups).extra


isOn : ExtraStatus -> Bool
isOn extraStatus =
    case extraStatus of
        OnOff on ->
            on

        OnOffLevel on _ ->
            on

        OnOffEco on _ ->
            on

        _ ->
            False


screenStatus : String -> Groups -> String
screenStatus name groups =
    (statusByName name groups).status


toggleDiv : ( String, String ) -> Model -> Html Msg
toggleDiv ( name, desc ) model =
    let
        record =
            statusByName name model.groups
    in
    case record.extra of
        OnOff status ->
            label []
                [ input [ type_ "checkbox", checked status, onClick (Clicked name) ] []
                , text (" " ++ desc)
                ]

        OnOffLevel _ _ ->
            toggleWithSliderDiv ( name, desc ) model

        OnOffEco status eco ->
            div [ style "display" "flex", style "gap" "20px", style "align-items" "center" ]
                [ label []
                    [ input [ type_ "checkbox", checked status, onClick (Clicked name) ] []
                    , text (" " ++ desc)
                    ]
                , label []
                    [ input [ type_ "checkbox", checked eco, onClick (ClickedEco name) ] []
                    , text " eco"
                    ]
                ]

        _ ->
            text ("BUG toggleDiv for " ++ name)


viewSwitches : String -> Model -> Html Msg
viewSwitches groupName model =
    let
        generateSwitch : StatusRecord -> Html Msg
        generateSwitch status =
            case status.kind of
                "DimmedLamp" ->
                    toggleWithSliderDiv ( status.name, status.description ) model

                "Lamp" ->
                    toggleDiv ( status.name, status.description ) model

                _ ->
                    text "Error"

        switchStatuses =
            Dict.get groupName model.groups |> Maybe.withDefault []
    in
    div [] (List.map generateSwitch switchStatuses)


toggleWithSliderDiv : ( String, String ) -> Model -> Html Msg
toggleWithSliderDiv ( name, desc ) model =
    let
        on =
            isOnByName name model.groups

        level =
            levelByName name model.groups
    in
    div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "margin" "4px 0" ]
        [ label []
            [ input [ type_ "checkbox", checked on, onClick (Clicked name) ] []
            , text (" " ++ desc)
            ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , value (String.fromFloat level)
            , disabled (not on)
            , onInput (SliderMsg name)
            , style "width" "150px"
            ]
            []
        ]


screenDiv : ( String, String ) -> Model -> Html Msg
screenDiv ( name, desc ) model =
    div [ style "margin" "4px 0" ]
        [ button [ onClick (Down name), style "margin-right" "4px" ] [ text "↓" ]
        , button [ onClick (Up name), style "margin-right" "8px" ] [ text "↑" ]
        , text (screenStatus name model.groups)
        , text (" | " ++ desc)
        ]


viewWindMeter : StatusRecord -> Html Msg
viewWindMeter statusRecord =
    let
        ( level, meterAttrs ) =
            case statusRecord.extra of
                Level l m ->
                    ( l, m )

                _ ->
                    ( 0, MeterAttributes 0 0 0 0 )
    in
    div []
        [ text "Wind: "
        , meter
            [ style "width" "250px"
            , style "height" "15px"
            , Html.Attributes.value (String.fromInt level)
            , attribute "optimum" "0"
            , Html.Attributes.min (String.fromInt meterAttrs.min)
            , attribute "low" (String.fromInt meterAttrs.low)
            , attribute "high" (String.fromInt meterAttrs.high)
            , Html.Attributes.max (String.fromInt meterAttrs.max)
            ]
            []
        , text (String.fromFloat (toFloat level / 100.0) ++ "RPM - " ++ statusRecord.status)
        ]


viewLightMeter : StatusRecord -> Html Msg
viewLightMeter statusRecord =
    let
        ( level, meterAttrs ) =
            case statusRecord.extra of
                Level l m ->
                    ( l, m )

                _ ->
                    ( 0, MeterAttributes 0 0 0 0 )
    in
    div []
        [ text "Zon: "
        , meter
            [ style "width" "250px"
            , style "height" "15px"
            , Html.Attributes.value (String.fromInt level)
            , attribute "optimum" "0"
            , Html.Attributes.min (String.fromInt meterAttrs.min)
            , attribute "low" (String.fromInt meterAttrs.high)
            , attribute "high" (String.fromInt meterAttrs.max)
            , Html.Attributes.max (String.fromInt meterAttrs.max)
            ]
            []
        , text (String.fromInt level ++ " - " ++ statusRecord.status)
        ]


viewScreens : String -> Model -> Html Msg
viewScreens groupName model =
    let
        statuses =
            Dict.get groupName model.groups |> Maybe.withDefault (List.singleton initialStatus)

        auto =
            List.filter (\s -> s.kind == "SunWindController") statuses

        autoHtml =
            case auto of
                [] ->
                    []

                first :: _ ->
                    [ toggleDiv ( first.name, first.description ) model ]

        wind =
            List.filter (\s -> s.kind == "WindSensor") statuses

        windHtml =
            case wind of
                [] ->
                    []

                first :: _ ->
                    [ viewWindMeter first ]

        light =
            List.filter (\s -> s.kind == "LightSensor") statuses

        lightHtml =
            case light of
                [] ->
                    []

                first :: _ ->
                    [ viewLightMeter first ]

        screens =
            List.filter (\s -> s.kind == "Screen") statuses
                |> List.map (\s -> screenDiv ( s.name, s.description ) model)
    in
    div [] (autoHtml ++ windHtml ++ lightHtml ++ screens)


somethingOn : Model -> String -> Bool
somethingOn model groupName =
    let
        groupStatuses =
            Dict.get groupName model.groups |> Maybe.withDefault []
    in
    List.foldl (\status soFar -> isOn status.extra || soFar) False groupStatuses


colorOfBlock : Model -> String -> String
colorOfBlock model groupName =
    if somethingOn model groupName then
        "orange"
    else
        "green"


groupToggleBar : String -> Model -> Html Msg
groupToggleBar groupName model =
    div
        [ style "background-color" (colorOfBlock model groupName)
        , style "width" "250px"
        , style "margin" "0px 0px 10px 0px"
        , style "padding" "10px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "gap" "10px"
        ]
        [ button [ onClick (ToggleShowBlock groupName) ]
            [ text
                (if isGroupOpen model.group2Expanded groupName then
                    "Verberg"
                 else
                    "Toon"
                )
            ]
        , span [ style "font-size" "120%" ] [ text groupName ]
        ]


viewGroup : (String -> Model -> Html Msg) -> String -> Model -> Html Msg
viewGroup subView groupName model =
    let
        content =
            if isGroupOpen model.group2Expanded groupName then
                subView groupName model
            else
                div [] []
    in
    div []
        [ groupToggleBar groupName model
        , content
        ]


viewErrorMsg : Maybe String -> Html Msg
viewErrorMsg msg =
    case msg of
        Nothing ->
            div [] []

        Just m ->
            div [ style "color" "DarkRed" ]
                [ text "Error: "
                , button [ onClick ClearErrorMessage ] [ text "Clear" ]
                , text m
                ]


viewTestMsg : Maybe String -> Html Msg
viewTestMsg msg =
    case msg of
        Nothing ->
            div [] []

        Just m ->
            div [ style "background" "DarkSlateGrey", style "color" "white" ]
                [ text "Test: "
                , button [ onClick ClearTestMessage ] [ text "Clear" ]
                , text m
                ]


view : Model -> Html Msg
view model =
    div [ style "padding" "2rem", style "background" "azure" ]
        [ div [ style "margin-bottom" "1rem" ]
            [ button [ onClick CollapseAllGroups, style "margin-right" "8px" ] [ text "Collapse All" ]
            , button [ onClick OpenAllGroups, style "margin-right" "8px" ] [ text "Open All" ]
            , button [ onClick PutModelInTestAsString ] [ text "Show Model" ]
            ]
        , viewGroup viewScreens "ScreensZ" model
        , viewGroup viewScreens "ScreensW" model
        , viewGroup viewSwitches "Beneden" model
        , viewGroup viewSwitches "Nutsruimtes" model
        , viewGroup viewSwitches "Kinderen" model
        , viewGroup viewSwitches "Buiten" model
        , hr [] []
        , viewErrorMsg model.errorMsg
        , viewTestMsg model.testMsg
        ]



----------------------------------------------------------
-- HELPERS


{-| Group equal elements together using a custom equality function.
Elements will be grouped in the same order as they appear in the original list.
The same applies to elements within each group.
    gatherWith (==) [1,2,1,3,2]
    --> [(1,[1]),(2,[2]),(3,[])]
-}
gatherWith : (a -> a -> Bool) -> List a -> List ( a, List a )
gatherWith testFn list =
    let
        helper : List a -> List ( a, List a ) -> List ( a, List a )
        helper scattered gathered =
            case scattered of
                [] ->
                    List.reverse gathered

                toGather :: population ->
                    let
                        ( gathering, remaining ) =
                            List.partition (testFn toGather) population
                    in
                    helper remaining <| ( toGather, gathering ) :: gathered
    in
    helper list []
