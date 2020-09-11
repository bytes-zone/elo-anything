module LeagueTest exposing (..)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import League exposing (League)
import Player
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
        , fuzz leagueFuzzer "is backwards-compatible with the older dictionary format" <|
            \league ->
                league.players
                    |> Encode.dict identity Player.encode
                    |> Decode.decodeValue League.decoder
                    |> Expect.equal (Ok league)
        ]


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.map League
        (Fuzz.list playerFuzzer
            |> Fuzz.map (List.map (\player -> ( player.name, player )) >> Dict.fromList)
        )
