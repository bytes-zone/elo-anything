module PlayerTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Player exposing (Player)
import Test exposing (..)


roundTripDecoderTest : Test
roundTripDecoderTest =
    fuzz playerFuzzer "encode and decode are symmetrical" <|
        \player ->
            Player.encode player
                |> Decode.decodeValue Player.decoder
                |> Expect.equal (Ok player)


playerFuzzer : Fuzzer Player
playerFuzzer =
    Fuzz.map3 Player
        Fuzz.string
        (Fuzz.intRange 1000 3000)
        (Fuzz.intRange 0 50)
