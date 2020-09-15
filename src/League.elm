module League exposing
    ( League, init, decoder, encode
    , addPlayer, players, getPlayer, retirePlayer
    , Match(..), currentMatch, nextMatch, startMatch, Outcome(..), finishMatch
    )

{-|

@docs League, init, decoder, encode

@docs addPlayer, players, getPlayer, retirePlayer

@docs Match, currentMatch, nextMatch, startMatch, Outcome, finishMatch

-}

import Dict exposing (Dict)
import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Player exposing (Player)
import Random exposing (Generator)


type League
    = League
        { players : Dict String Player
        , matchesPlayed : Int
        , currentMatch : Maybe Match
        }


type Match
    = Match Player Player



-- LOADING AND SAVING


init : League
init =
    League
        { players = Dict.empty
        , matchesPlayed = 0
        , currentMatch = Nothing
        }


decoder : Decoder League
decoder =
    Decode.oneOf
        [ Decode.map2
            (\newPlayers matchesPlayed ->
                League
                    { players = newPlayers
                    , matchesPlayed = matchesPlayed
                    , currentMatch = Nothing
                    }
            )
            playersDecoder
            (Decode.field "matchesPlayed" Decode.int)
        , -- old formats
          Decode.map
            (\newPlayers ->
                League
                    { players = newPlayers
                    , matchesPlayed =
                        newPlayers
                            |> Dict.values
                            |> List.map .matches
                            |> List.maximum
                            |> Maybe.withDefault 0
                    , currentMatch = Nothing
                    }
            )
            (Decode.oneOf
                [ -- old format:  : missing matches played
                  playersDecoder
                , -- old format: only players as a dict
                  Decode.dict Player.decoder
                ]
            )
        ]


playersDecoder : Decoder (Dict String Player)
playersDecoder =
    Decode.field "players" (Decode.list Player.decoder)
        |> Decode.map (List.map (\player -> ( player.name, player )))
        |> Decode.map Dict.fromList


encode : League -> Encode.Value
encode (League league) =
    Encode.object
        [ ( "players", Encode.list Player.encode (Dict.values league.players) )
        , ( "matchesPlayed", Encode.int league.matchesPlayed )
        ]



-- PLAYERS


players : League -> List Player
players (League league) =
    Dict.values league.players


getPlayer : String -> League -> Maybe Player
getPlayer name (League league) =
    Dict.get name league.players


addPlayer : Player -> League -> League
addPlayer player (League league) =
    League { league | players = Dict.insert player.name player league.players }


{-| Chesterton's export
-}
updatePlayer : Player -> League -> League
updatePlayer =
    addPlayer


retirePlayer : Player -> League -> League
retirePlayer player (League league) =
    League
        { league
            | players = Dict.remove player.name league.players
            , currentMatch =
                case league.currentMatch of
                    Nothing ->
                        Nothing

                    Just (Match a b) ->
                        if player.name == a.name || player.name == b.name then
                            Nothing

                        else
                            league.currentMatch
        }



-- MATCHES


currentMatch : League -> Maybe Match
currentMatch (League league) =
    league.currentMatch


{-| Select the next match according to a two-phase system:

1.  If there are players who have less than the "play-in" number of matches
    (that is, the number of matches I feel are needed to get a good idea of
    the player's rough ranking) then choose among them randomly. If there
    are no such players then choose among all the players, favoring players
    who have played less recently.

2.  Once the first player is chosen, choose a second player close to them
    by rank. The ideal matchup goes from a tie to a decisive "this player
    is ranked higher."

Edge case: If there are fewer than two unique players, we can't schedule a
new match.

-}
nextMatch : League -> Generator (Maybe Match)
nextMatch (League league) =
    let
        playInMatches =
            5

        allPlayers =
            Dict.values league.players
    in
    case allPlayers of
        -- at least two
        a :: b :: rest ->
            (case List.filter (\player -> player.matches <= playInMatches) allPlayers of
                firstPlayIn :: restOfPlayIns ->
                    Random.uniform firstPlayIn restOfPlayIns

                _ ->
                    let
                        mostMatches =
                            List.map .matches allPlayers
                                |> List.maximum
                                |> Maybe.withDefault 0
                    in
                    Random.weighted
                        ( toFloat (mostMatches - a.matches) ^ 2, a )
                        (List.map (\player -> ( toFloat (mostMatches - a.matches) ^ 2, player )) (b :: rest))
            )
                |> Random.andThen
                    (\firstPlayer ->
                        let
                            ( head, tail ) =
                                if firstPlayer == a then
                                    ( b, rest )

                                else if firstPlayer == b then
                                    ( a, rest )

                                else
                                    ( a, b :: List.filter (\p -> p /= firstPlayer) rest )

                            furthestAway =
                                (head :: tail)
                                    |> List.map (\player -> abs (firstPlayer.rating - player.rating))
                                    |> List.maximum
                                    |> Maybe.withDefault 0
                        in
                        Random.weighted
                            ( toFloat (furthestAway - abs (firstPlayer.rating - head.rating)) ^ 2, head )
                            (List.map (\player -> ( toFloat (furthestAway - abs (firstPlayer.rating - player.rating)) ^ 2, player )) tail)
                            |> Random.map (Tuple.pair firstPlayer)
                    )
                |> Random.andThen
                    (\( playerA, playerB ) ->
                        Random.map
                            (\flip ->
                                if flip then
                                    Match playerA playerB

                                else
                                    Match playerB playerA
                            )
                            (Random.uniform True [ False ])
                    )
                |> Random.map Just

        -- one or zero players
        _ ->
            Random.constant Nothing


startMatch : Match -> League -> League
startMatch (Match playerA playerB) (League league) =
    League
        { league
            | currentMatch =
                -- don't start a match with players that aren't in the
                -- league...
                Maybe.map2 Tuple.pair
                    (Dict.get playerA.name league.players)
                    (Dict.get playerB.name league.players)
                    |> Maybe.andThen
                        (\( gotA, gotB ) ->
                            -- ... or when the players are the same player
                            if gotA /= gotB then
                                Just (Match gotA gotB)

                            else
                                Nothing
                        )
        }


type Outcome
    = Win { won : Player, lost : Player }
    | Draw { playerA : Player, playerB : Player }


finishMatch : Outcome -> League -> League
finishMatch outcome league =
    case outcome of
        Win { won, lost } ->
            let
                newRatings =
                    Elo.win Elo.sensitiveKFactor
                        { won = won.rating
                        , lost = lost.rating
                        }
            in
            league
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.won won))
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.lost lost))
                |> clearMatch

        Draw { playerA, playerB } ->
            let
                newRatings =
                    Elo.draw Elo.sensitiveKFactor
                        { playerA = playerA.rating
                        , playerB = playerB.rating
                        }
            in
            league
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.playerA playerA))
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.playerB playerB))
                |> clearMatch


{-| Chesterton's export
-}
clearMatch : League -> League
clearMatch (League league) =
    League { league | currentMatch = Nothing }
