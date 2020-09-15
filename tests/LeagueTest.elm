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


initTests : Test
initTests =
    describe "init"
        [ test "starts with no players" <|
            \_ ->
                League.init
                    |> League.players
                    |> Expect.equal []
        , test "starts without a current match" <|
            \_ ->
                League.init
                    |> League.currentMatch
                    |> Expect.equal Nothing
        ]


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
                League.players league
                    |> List.map (\player -> ( player.name, Player.encode player ))
                    |> Encode.object
                    |> Decode.decodeValue League.decoder
                    |> Expect.equal (Ok league)
        ]


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.list playerFuzzer
        |> Fuzz.map (List.foldr League.addPlayer League.init)
