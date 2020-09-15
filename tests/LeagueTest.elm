module LeagueTest exposing (..)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import League exposing (League, Match(..), Outcome(..))
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



-- PLAYERS


playersTests : Test
playersTests =
    describe "functionality around players"
        [ fuzz playerFuzzer "adding a player makes them show up in the players list" <|
            \player ->
                League.init
                    |> League.addPlayer player
                    |> League.players
                    |> Expect.equal [ player ]
        , fuzz playerFuzzer "retiring a player removes them from the players list" <|
            \player ->
                League.init
                    |> League.addPlayer player
                    |> League.retirePlayer player
                    |> League.players
                    |> Expect.equal []
        ]



-- MATCHES


startMatchTests : Test
startMatchTests =
    describe "startMatch"
        [ fuzz2 playerFuzzer playerFuzzer "you can't start a match when neither player is in the league" <|
            \playerA playerB ->
                League.init
                    |> League.startMatch (Match playerA playerB)
                    |> League.currentMatch
                    |> Expect.equal Nothing
        , fuzz2 playerFuzzer playerFuzzer "you can't start a match when one player isn't in the league" <|
            \playerA playerB ->
                let
                    uniqueA =
                        { playerA | name = "real " ++ playerA.name }

                    uniqueB =
                        { playerB | name = "fake " ++ playerB.name }
                in
                League.init
                    |> League.addPlayer uniqueA
                    |> League.startMatch (Match uniqueA uniqueB)
                    |> League.currentMatch
                    |> Expect.equal Nothing
        , fuzz2 playerFuzzer playerFuzzer "you can start a match between two players in the league" <|
            \playerA playerBMaybeSame ->
                let
                    playerB =
                        { playerBMaybeSame | name = "unique " ++ playerBMaybeSame.name }
                in
                League.init
                    |> League.addPlayer playerA
                    |> League.addPlayer playerB
                    |> League.startMatch (Match playerA playerB)
                    |> League.currentMatch
                    |> Expect.equal (Just (Match playerA playerB))
        , fuzz playerFuzzer "you can't start a match with one player against themselves" <|
            \player ->
                League.init
                    |> League.addPlayer player
                    |> League.startMatch (Match player player)
                    |> League.currentMatch
                    |> Expect.equal Nothing
        ]


finishMatchTests : Test
finishMatchTests =
    let
        dummy =
            Player.init "A"

        league =
            League.init
                |> League.addPlayer dummy
    in
    describe "finishMatch"
        [ fuzz playerFuzzer "a win causes both players matches played to go up" <|
            \winner ->
                league
                    |> League.addPlayer winner
                    |> League.startMatch (Match winner dummy)
                    |> League.finishMatch (Win { won = winner, lost = dummy })
                    |> Expect.all
                        [ League.getPlayer winner.name
                            >> Maybe.map .matches
                            >> Expect.equal (Just (winner.matches + 1))
                        , League.getPlayer dummy.name
                            >> Maybe.map .matches
                            >> Expect.equal (Just (dummy.matches + 1))
                        ]
        , fuzz playerFuzzer "a win causes the winning player's score to go up" <|
            \winner ->
                league
                    |> League.addPlayer winner
                    |> League.startMatch (Match winner dummy)
                    |> League.finishMatch (Win { won = winner, lost = dummy })
                    |> League.getPlayer winner.name
                    |> Maybe.map .rating
                    |> Maybe.withDefault 0
                    -- if the score difference is too great, the winning
                    -- player won't gain any points. So really we need to check
                    -- that the score *doesn't go down.*
                    |> Expect.atLeast winner.rating
        , fuzz playerFuzzer "a win causes the losing player's score to go down" <|
            \winner ->
                league
                    |> League.addPlayer winner
                    |> League.startMatch (Match winner dummy)
                    |> League.finishMatch (Win { won = winner, lost = dummy })
                    |> League.getPlayer dummy.name
                    |> Maybe.map .rating
                    |> Maybe.withDefault 0
                    -- if the score difference is too great, the losing
                    -- player won't lose any points. So really we need to check
                    -- that the score *doesn't go down.*
                    |> Expect.atMost dummy.rating
        , fuzz playerFuzzer "a win does not change the total points in the system" <|
            \winner ->
                league
                    |> League.addPlayer winner
                    |> League.startMatch (Match winner dummy)
                    |> League.finishMatch (Win { won = winner, lost = dummy })
                    |> League.players
                    |> List.map .rating
                    |> List.sum
                    |> Expect.equal (winner.rating + dummy.rating)
        , fuzz playerFuzzer "a draw causes both players matches played to go up" <|
            \player ->
                league
                    |> League.addPlayer player
                    |> League.startMatch (Match player dummy)
                    |> League.finishMatch (Draw { playerA = player, playerB = dummy })
                    |> Expect.all
                        [ League.getPlayer player.name
                            >> Maybe.map .matches
                            >> Expect.equal (Just (player.matches + 1))
                        , League.getPlayer dummy.name
                            >> Maybe.map .matches
                            >> Expect.equal (Just (dummy.matches + 1))
                        ]
        , fuzz playerFuzzer "a draw does not change the total points in the system" <|
            \player ->
                league
                    |> League.addPlayer player
                    |> League.startMatch (Match player dummy)
                    |> League.finishMatch (Draw { playerA = player, playerB = dummy })
                    |> League.players
                    |> List.map .rating
                    |> List.sum
                    |> Expect.equal (player.rating + dummy.rating)
        ]



-- FUZZERS


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.list playerFuzzer
        |> Fuzz.map (List.foldr League.addPlayer League.init)
