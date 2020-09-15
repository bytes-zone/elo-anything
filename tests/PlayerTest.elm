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
        nameFuzzer
        (Fuzz.intRange 1000 3000)
        (Fuzz.intRange 0 50)


nameFuzzer : Fuzzer String
nameFuzzer =
    let
        chars =
            Fuzz.intRange (Char.toCode 'a') (Char.toCode 'c')
                |> Fuzz.map Char.fromCode
    in
    Fuzz.map2 (::) chars (Fuzz.list chars)
        |> Fuzz.map String.fromList
