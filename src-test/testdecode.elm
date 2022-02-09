module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode exposing (..)


-- TYPES ===============================


type alias StatusRecord =
    { name : String, kind : String, group : String, description : String, on : Bool, level : Int, status : String }




-- DECODING ===============================
-- Decodes a Json string into list of StatusRecords


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



-- Decoder: decodes the list


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





-- HELPER ===============================
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


emptyStatus : StatusRecord
emptyStatus =
    { name = "", kind = "", group = "", description = "", on = False, level = 0, status = "" }



-- TEST ===============================


status : String
status =
    """
[
  {
    "name": "Windmeter",
    "type": "WindSensor",
    "description": "",
    "groupName": "SPECIAAL:2",
    "on": false,
    "level": 186,
    "status": "NORMAL"
  },
  {
    "name": "Lichtmeter",
    "type": "LightSensor",
    "description": "",
    "groupName": "SPECIAAL:3",
    "on": false,
    "level": 3041,
    "status": "HIGH2LOW_DELAY"
  },
  {
    "name": "ZonWindAutomaat",
    "type": "SunWindController",
    "description": "Zon/Wind Automaat",
    "groupName": "Screens:0",
    "on": false,
    "level": 0,
    "status": "OFF"
  },
  {
    "name": "LichtInkom",
    "type": "Lamp",
    "description": "Inkom",
    "groupName": "Nutsruimtes:0",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtWC0",
    "type": "Lamp",
    "description": "WC",
    "groupName": "Nutsruimtes:6",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "VentilatorWC0",
    "type": "Fan",
    "description": "Ventilator WC",
    "groupName": "",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtGarageTuin",
    "type": "Lamp",
    "description": "Garage (Tuin)",
    "groupName": "Nutsruimtes:3",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtGaragePoort",
    "type": "Lamp",
    "description": "Garage (Poort)",
    "groupName": "Nutsruimtes:2",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtKeuken",
    "type": "Lamp",
    "description": "Keuken",
    "groupName": "Beneden:0",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtBadk0",
    "type": "Lamp",
    "description": "Badkamer +0",
    "groupName": "Nutsruimtes:4",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "VentilatorBadk0",
    "type": "Fan",
    "description": "Ventilator Badkamer",
    "groupName": "",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtVeranda",
    "type": "DimmedLamp",
    "description": "Veranda",
    "groupName": "Beneden:1",
    "on": true,
    "level": 26,
    "status": ""
  },
  {
    "name": "LichtCircanteRondom",
    "type": "DimmedLamp",
    "description": "Eetkamer",
    "groupName": "Beneden:2",
    "on": true,
    "level": 100,
    "status": ""
  },
  {
    "name": "LichtCircante",
    "type": "Lamp",
    "description": "Circante Tafel",
    "groupName": "Beneden:3",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtZithoek",
    "type": "DimmedLamp",
    "description": "Zithoek",
    "groupName": "Beneden:4",
    "on": true,
    "level": 40,
    "status": ""
  },
  {
    "name": "LichtBureau",
    "type": "Lamp",
    "description": "Bureau",
    "groupName": "Beneden:5",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtGangBoven",
    "type": "Lamp",
    "description": "Gang Boven",
    "groupName": "Nutsruimtes:1",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtBadk1",
    "type": "Lamp",
    "description": "Badkamer +1",
    "groupName": "Nutsruimtes:5",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtTomasWand",
    "type": "Lamp",
    "description": "Tomas Wand",
    "groupName": "Tomas:0",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtTomasSpots",
    "type": "Lamp",
    "description": "Tomas Spots",
    "groupName": "Tomas:1",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtDriesWand",
    "type": "Lamp",
    "description": "Dries Wand",
    "groupName": "Dries:0",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtDries",
    "type": "Lamp",
    "description": "Dries Spots",
    "groupName": "Dries:1",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtRoosWand",
    "type": "Lamp",
    "description": "Roos Wand",
    "groupName": "Roos:0",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtRoos",
    "type": "Lamp",
    "description": "Roos Spots",
    "groupName": "Roos:1",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtTerras",
    "type": "Lamp",
    "description": "Licht terras en zijkant",
    "groupName": "Buiten:0",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "StopkBuiten",
    "type": "Lamp",
    "description": "Stopcontact buiten",
    "groupName": "Buiten:1",
    "on": false,
    "level": 0,
    "status": ""
  },
  {
    "name": "LichtNacht",
    "type": "Lamp",
    "description": "Nachtlichten",
    "groupName": "",
    "on": true,
    "level": 0,
    "status": ""
  },
  {
    "name": "ScreenTomas",
    "type": "Screen",
    "description": "Tomas",
    "groupName": "Screens:3",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenDriesTuin",
    "type": "Screen",
    "description": "Dries (T)",
    "groupName": "Screens:4",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenDriesOpzij",
    "type": "Screen",
    "description": "Dries (Z)",
    "groupName": "Screens:5",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenRoos",
    "type": "Screen",
    "description": "Roos",
    "groupName": "Screens:6",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenBreed",
    "type": "Screen",
    "description": "Breed",
    "groupName": "Screens:1",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenLang",
    "type": "Screen",
    "description": "Smal",
    "groupName": "Screens:2",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  },
  {
    "name": "ScreenKeuken",
    "type": "Screen",
    "description": "Keuken",
    "groupName": "Screens:7",
    "on": false,
    "level": 0,
    "status": "0% OPEN"
  }
]
"""


main =
    --text ("Decoded: " ++ (toString (decodeStatuses status)))
    let
        ( value, _ ) =
            decodeStatuses status

        wind =
            statusByName "Windmeter" value

        sun =
            statusByName "Zonnesensor" value
    in
        text ("Wind : " ++ (toString wind) ++ " |    Sun:" ++ (toString sun))
