module LeagueTest exposing (..)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import League exposing (League)
import PlayerTest exposing (playerFuzzer)
import Test exposing (..)


decoderTests : Test
decoderTests =
    describe "decoder"
        [ fuzz leagueFuzzer "encode and decode are symmetrical" <|
            \league ->
                League.encode league
                    |> Decode.decodeValue League.decoder
                    |> Expect.equal (Ok league)
        ]


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.map League
        (Fuzz.list playerFuzzer
            |> Fuzz.map (List.map (\player -> ( player.name, player )) >> Dict.fromList)
        )
