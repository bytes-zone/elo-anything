module PlayerTest exposing (..)

import Elo
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
        [ fuzz establishedPlayerFuzzer "encode and decode are symmetrical" <|
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


nameTest : Test
nameTest =
    describe "name"
        [ test "you get the same name out as you put in" <|
            \_ ->
                Player.init "Babaganoush"
                    |> Player.name
                    |> Expect.equal "Babaganoush"
        ]


matchesTest : Test
matchesTest =
    describe "matches"
        [ test "you start off having played zero matches" <|
            \_ ->
                Player.init "Pita"
                    |> Player.matchesPlayed
                    |> Expect.equal 0
        , test "when you play a match, you can see it" <|
            \_ ->
                Player.init "Pita"
                    |> Player.incrementMatchesPlayed
                    |> Player.matchesPlayed
                    |> Expect.equal 1
        ]


ratingTest : Test
ratingTest =
    describe "rating"
        [ test "you start off at the default Elo rating" <|
            \_ ->
                Player.init "Shish Taouk"
                    |> Player.rating
                    |> Expect.equal Elo.initialRating
        , fuzz (Fuzz.intRange 0 (Elo.initialRating * 2)) "your rating can be set to whatever" <|
            \rating ->
                Player.init "Shish Taouk"
                    |> Player.setRating rating
                    |> Player.rating
                    |> Expect.equal rating
        , test "your rating cannot go below zero" <|
            \_ ->
                Player.init "Shish Taouk"
                    |> Player.setRating -1
                    |> Player.rating
                    |> Expect.equal 0
        ]


establishedPlayerFuzzer : Fuzzer Player
establishedPlayerFuzzer =
    Fuzz.map3
        (\name rating matches ->
            Player.init name
                |> Player.setRating rating
                |> Player.setMatchesPlayedTestOnly matches
        )
        nameFuzzer
        (Fuzz.intRange 1000 3000)
        (Fuzz.intRange 5 50)


nameFuzzer : Fuzzer String
nameFuzzer =
    Fuzz.intRange (Char.toCode 'a') (Char.toCode 'c')
        |> Fuzz.map Char.fromCode
        |> Fuzz.map String.fromChar
