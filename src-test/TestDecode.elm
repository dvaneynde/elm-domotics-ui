module TestDecode exposing (..)

-- Automated tests for Domotic.statusesDecoder using elm-explorations/test.
-- Run with: npx elm-test src-test/TestDecode.elm

import Domotic exposing (ExtraStatus(..), MeterAttributes, statusesDecoder)
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Domotic.statusesDecoder"
        [ test "decodes empty list" <|
            \_ ->
                Decode.decodeString statusesDecoder "[]"
                    |> Expect.equal (Ok [])
        , test "decodes a Lamp with OnOff extra" <|
            \_ ->
                let
                    json =
                        """[{"name":"LichtInkom","type":"Lamp","groupName":"Nutsruimtes","groupSeq":0,"description":"Inkom","status":"","on":false}]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok [ record ] ->
                        Expect.all
                            [ \r -> Expect.equal "LichtInkom" r.name
                            , \r -> Expect.equal "Lamp" r.kind
                            , \r -> Expect.equal "Nutsruimtes" r.groupName
                            , \r -> Expect.equal (OnOff False) r.extra
                            ]
                            record

                    Ok _ ->
                        Expect.fail "Expected exactly one record"
        , test "decodes a DimmedLamp with OnOffLevel extra" <|
            \_ ->
                let
                    json =
                        """[{"name":"LichtVeranda","type":"DimmedLamp","groupName":"Beneden","groupSeq":1,"description":"Veranda","status":"","on":true,"level":26}]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok [ record ] ->
                        Expect.equal (OnOffLevel True 26) record.extra

                    Ok _ ->
                        Expect.fail "Expected exactly one record"
        , test "decodes a WindSensor with Level and meter attributes" <|
            \_ ->
                let
                    json =
                        """[{"name":"Windmeter","type":"WindSensor","groupName":"SPECIAAL","groupSeq":2,"description":"","status":"NORMAL","level":186,"min":0,"low":100,"high":200,"max":300}]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok [ record ] ->
                        Expect.equal (Level 186 (MeterAttributes 0 100 200 300)) record.extra

                    Ok _ ->
                        Expect.fail "Expected exactly one record"
        , test "decodes an OnOffEco device" <|
            \_ ->
                let
                    json =
                        """[{"name":"Verwarming","type":"Thermostat","groupName":"Beneden","groupSeq":5,"description":"Verwarming","status":"","on":true,"eco":false}]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok [ record ] ->
                        Expect.equal (OnOffEco True False) record.extra

                    Ok _ ->
                        Expect.fail "Expected exactly one record"
        , test "decodes a device with no extra fields as None" <|
            \_ ->
                let
                    json =
                        """[{"name":"Sensor","type":"Unknown","groupName":"","groupSeq":0,"description":"","status":""}]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok [ record ] ->
                        Expect.equal None record.extra

                    Ok _ ->
                        Expect.fail "Expected exactly one record"
        , test "returns an error for invalid JSON" <|
            \_ ->
                Decode.decodeString statusesDecoder "not json"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "decodes multiple records preserving order" <|
            \_ ->
                let
                    json =
                        """[
                          {"name":"First","type":"Lamp","groupName":"A","groupSeq":0,"description":"","status":"","on":false},
                          {"name":"Second","type":"Lamp","groupName":"A","groupSeq":1,"description":"","status":"","on":true}
                        ]"""
                in
                case Decode.decodeString statusesDecoder json of
                    Err e ->
                        Expect.fail (Decode.errorToString e)

                    Ok records ->
                        Expect.equal [ "First", "Second" ] (List.map .name records)
        ]
