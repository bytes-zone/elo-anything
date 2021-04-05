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
                    |> List.map (\player -> ( Player.name player, Player.encode player ))
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
                    |> List.map Player.name
                    |> Expect.equal [ Player.name player ]
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
                        Player.init ("real " ++ Player.name playerA)

                    uniqueB =
                        Player.init ("real " ++ Player.name playerA)
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
                        Player.init ("unique " ++ Player.name playerBMaybeSame)
                in
                League.init
                    |> League.addPlayer playerA
                    |> League.addPlayer playerB
                    |> League.startMatch (Match playerA playerB)
                    |> League.currentMatch
                    |> Maybe.map (\(Match a b) -> ( Player.name a, Player.name b ))
                    |> Expect.equal (Just ( Player.name playerA, Player.name playerB ))
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
                            [ League.getPlayer (Player.id winner)
                                >> Maybe.map Player.matchesPlayed
                                >> Expect.equal (Just (Player.matchesPlayed winner + 1))
                            , League.getPlayer (Player.id existingPlayer)
                                >> Maybe.map Player.matchesPlayed
                                >> Expect.equal (Just (Player.matchesPlayed existingPlayer + 1))
                            ]
            , fuzz playerFuzzer "changes ratings according to Elo" <|
                \winner ->
                    let
                        newRatings =
                            Elo.win (League.kFactor league winner)
                                { won = Player.rating winner
                                , lost = Player.rating existingPlayer
                                }
                    in
                    league
                        |> League.addPlayer winner
                        |> League.startMatch (Match winner existingPlayer)
                        |> League.finishMatch (Win { won = winner, lost = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer (Player.id winner)
                                >> Maybe.map Player.rating
                                >> Expect.equal (Just newRatings.won)
                            , League.getPlayer (Player.id existingPlayer)
                                >> Maybe.map Player.rating
                                >> Expect.equal (Just newRatings.lost)
                            ]
            , fuzz playerFuzzer "does not change the total points in the system" <|
                \winner ->
                    league
                        |> League.addPlayer winner
                        |> League.startMatch (Match winner existingPlayer)
                        |> League.finishMatch (Win { won = winner, lost = existingPlayer })
                        |> League.players
                        |> List.map Player.rating
                        |> List.sum
                        |> Expect.equal (Player.rating winner + Player.rating existingPlayer)
            ]
        , describe "a draw"
            [ fuzz playerFuzzer "a draw causes both players matches played to go up" <|
                \player ->
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer (Player.id player)
                                >> Maybe.map Player.matchesPlayed
                                >> Expect.equal (Just (Player.matchesPlayed player + 1))
                            , League.getPlayer (Player.id existingPlayer)
                                >> Maybe.map Player.matchesPlayed
                                >> Expect.equal (Just (Player.matchesPlayed existingPlayer + 1))
                            ]
            , fuzz playerFuzzer "a draw changes ratings according to Elo" <|
                \player ->
                    let
                        newRatings =
                            Elo.draw
                                (League.kFactor league
                                    (if Player.rating player > Player.rating existingPlayer then
                                        player

                                     else
                                        existingPlayer
                                    )
                                )
                                { playerA = Player.rating player
                                , playerB = Player.rating existingPlayer
                                }
                    in
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> Expect.all
                            [ League.getPlayer (Player.id player)
                                >> Maybe.map Player.rating
                                >> Expect.equal (Just newRatings.playerA)
                            , League.getPlayer (Player.id existingPlayer)
                                >> Maybe.map Player.rating
                                >> Expect.equal (Just newRatings.playerB)
                            ]
            , fuzz playerFuzzer "a draw does not change the total points in the system" <|
                \player ->
                    league
                        |> League.addPlayer player
                        |> League.startMatch (Match player existingPlayer)
                        |> League.finishMatch (Draw { playerA = player, playerB = existingPlayer })
                        |> League.players
                        |> List.map Player.rating
                        |> List.sum
                        |> Expect.equal (Player.rating player + Player.rating existingPlayer)
            ]
        ]



-- FUZZERS


leagueFuzzer : Fuzzer League
leagueFuzzer =
    Fuzz.list playerFuzzer
        |> Fuzz.map (List.foldr League.addPlayer League.init)
