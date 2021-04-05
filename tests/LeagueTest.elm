module LeagueTest exposing (..)

import Dict exposing (Dict)
import Elo
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
                    -- matches played will change with this. That's fine.
                    |> Result.map League.players
                    |> Expect.equal (Ok (League.players league))
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
                    |> List.map .name
                    |> Expect.equal [ player.name ]
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
                        Player.init ("real " ++ playerA.name)

                    uniqueB =
                        Player.init ("real " ++ playerA.name)
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
                        Player.init ("unique " ++ playerBMaybeSame.name)
                in
                League.init
                    |> League.addPlayer playerA
                    |> League.addPlayer playerB
                    |> League.startMatch (Match playerA playerB)
                    |> League.currentMatch
                    |> Maybe.map (\(Match a b) -> ( a.name, b.name ))
                    |> Expect.equal (Just ( playerA.name, playerB.name ))
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
        existingPlayer =
            Player.init "A"

        league =
            League.init
                |> League.addPlayer existingPlayer
    in
    describe "finishMatch"
        [ describe "a win"
            [ fuzz playerFuzzer "causes both players matches played to go up" <|
                \winner ->
                    league
                        |> League.addPlayer winner
                        |> League.startMatch (Match winner existingPlayer)
                        |> League.finishMatch (Win { won = winner, lost = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer winner.id
                                >> Maybe.map .matches
                                >> Expect.equal (Just (winner.matches + 1))
                            , League.getPlayer existingPlayer.id
                                >> Maybe.map .matches
                                >> Expect.equal (Just (existingPlayer.matches + 1))
                            ]
            , fuzz playerFuzzer "changes ratings according to Elo" <|
                \winner ->
                    let
                        newRatings =
                            Elo.win (League.kFactor league winner)
                                { won = winner.rating
                                , lost = existingPlayer.rating
                                }
                    in
                    league
                        |> League.addPlayer winner
                        |> League.startMatch (Match winner existingPlayer)
                        |> League.finishMatch (Win { won = winner, lost = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer winner.id
                                >> Maybe.map .rating
                                >> Expect.equal (Just newRatings.won)
                            , League.getPlayer existingPlayer.id
                                >> Maybe.map .rating
                                >> Expect.equal (Just newRatings.lost)
                            ]
            , fuzz playerFuzzer "does not change the total points in the system" <|
                \winner ->
                    league
                        |> League.addPlayer winner
                        |> League.startMatch (Match winner existingPlayer)
                        |> League.finishMatch (Win { won = winner, lost = existingPlayer })
                        |> League.players
                        |> List.map .rating
                        |> List.sum
                        |> Expect.equal (winner.rating + existingPlayer.rating)
            ]
        , describe "a draw"
            [ fuzz playerFuzzer "a draw causes both players matches played to go up" <|
                \player ->
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer player.id
                                >> Maybe.map .matches
                                >> Expect.equal (Just (player.matches + 1))
                            , League.getPlayer existingPlayer.id
                                >> Maybe.map .matches
                                >> Expect.equal (Just (existingPlayer.matches + 1))
                            ]
            , fuzz playerFuzzer "a draw changes ratings according to Elo" <|
                \player ->
                    let
                        newRatings =
                            Elo.draw
                                (League.kFactor league
                                    (if player.rating > existingPlayer.rating then
                                        player

                                     else
                                        existingPlayer
                                    )
                                )
                                { playerA = player.rating
                                , playerB = existingPlayer.rating
                                }
                    in
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer player.id
                                >> Maybe.map .rating
                                >> Expect.equal (Just newRatings.playerA)
                            , League.getPlayer existingPlayer.id
                                >> Maybe.map .rating
                                >> Expect.equal (Just newRatings.playerB)
                            ]
            , fuzz playerFuzzer "a draw does not change the total points in the system" <|
                \player ->
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> League.players
                        |> List.map .rating
                        |> List.sum
                        |> Expect.equal (player.rating + existingPlayer.rating)
            ]
        ]



-- FUZZERS


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.list playerFuzzer
        |> Fuzz.map (List.foldr League.addPlayer League.init)
