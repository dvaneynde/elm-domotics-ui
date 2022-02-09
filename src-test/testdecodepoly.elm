module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode exposing (..)


-- TYPES ===============================

type ExtraStatus =
    None | OnOff Bool | OnOffLevel Bool Int

type alias StatusRecord =
    { name : String, kind : String, group : String, description : String, status : String, extra : ExtraStatus }

emptyStatus : StatusRecord
emptyStatus =
    { name = "", kind = "", group = "", description = "", status = "", extra = None }


-- DECODING ===============================


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
    map6 StatusRecord
        (field "name" string)
        (field "type" string)
        (field "group" string)
        (field "description" string)
        (field "status" string)
        (oneOf [ decoderExtraOnOffLevel, decoderExtraOnOff, succeed None])


decoderExtraOnOffLevel : Decoder ExtraStatus
decoderExtraOnOffLevel =
    map2 OnOffLevel (field "on" bool) (field "level" int)


decoderExtraOnOff : Decoder ExtraStatus
decoderExtraOnOff =
    map OnOff (field "on" bool)



-- HELPERS ===============================

-- find StatusRecord by name
statusByName : String -> List StatusRecord -> StatusRecord
statusByName name listOfRecords =
    let
        checkName =
            (\rec -> rec.name == name)

        filteredList =
            List.filter checkName listOfRecords
    in
        Maybe.withDefault emptyStatus (List.head filteredList)




-- TEST ===============================


status : String
status =
    """
[
  {
    "name": "Windmeter",
    "type": "WindSensor",
    "description": "",
    "group": "SPECIAAL:2",
    "status": "ALARM",
    "on": true,
    "level": 925
  },
  {
    "name": "ZonWindAutomaat",
    "type": "SunWindController",
    "description": "Zon/Wind Automaat",
    "group": "Screens:0",
    "status": "",
    "on": false
  },
  {
    "name": "ScreenBreed",
    "type": "Screen",
    "description": "Breed",
    "group": "Screens:1",
    "status": "0% ^^^^ STORM"
  }
]
"""

{-
-}

main =
    --text ("Decoded: " ++ (toString (decodeStatuses status)))
    let
        ( value, error ) =
            decodeStatuses status

    in
        text ( "Error: " ++ error ++ "<br>, value: " ++ (toString value))
