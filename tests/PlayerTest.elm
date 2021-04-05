module PlayerTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import Murmur3
import Player exposing (Player)
import Test exposing (..)


interopTest : Test
interopTest =
    describe "interop"
        [ fuzz playerFuzzer "encode and decode are symmetrical" <|
            \player ->
                Player.encode player
                    |> Decode.decodeValue Player.decoder
                    |> Expect.equal (Ok player)
        , describe "decoder"
            [ describe "id"
                [ test "is OK with a missing ID" <|
                    \_ ->
                        Encode.object
                            [ ( "name", Encode.string "Test" )
                            , ( "rating", Encode.int 1200 )
                            , ( "matches", Encode.int 0 )
                            ]
                            |> Decode.decodeValue Player.decoder
                            |> Expect.ok
                ]
            ]
        ]


playerFuzzer : Fuzzer Player
playerFuzzer =
    Fuzz.map3
        (\name rating matches ->
            Player.init name
                |> Player.setRating rating
                |> Player.setMatchesPlayed matches
        )
        nameFuzzer
        (Fuzz.intRange 1000 3000)
        (Fuzz.intRange 0 50)


nameFuzzer : Fuzzer String
nameFuzzer =
    Fuzz.intRange (Char.toCode 'a') (Char.toCode 'c')
        |> Fuzz.map Char.fromCode
        |> Fuzz.map String.fromChar
