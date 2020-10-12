module League exposing
    ( League, init, decoder, encode
    , addPlayer, players, getPlayer, retirePlayer
    , Match(..), currentMatch, nextMatch, startMatch, Outcome(..), finishMatch, kFactor
    )

{-|

@docs League, init, decoder, encode

@docs addPlayer, players, getPlayer, retirePlayer

@docs Match, currentMatch, nextMatch, startMatch, Outcome, finishMatch, kFactor

-}

import Dict exposing (Dict)
import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Player exposing (Player)
import Random exposing (Generator)


type League
    = League
        { players : Dict String Player
        , currentMatch : Maybe Match
        }


type Match
    = Match Player Player



-- LOADING AND SAVING


init : League
init =
    League
        { players = Dict.empty
        , currentMatch = Nothing
        }


decoder : Decoder League
decoder =
    Decode.map
        (\newPlayers ->
            League
                { players = newPlayers
                , currentMatch = Nothing
                }
        )
        (Decode.oneOf
            [ playersDecoder
            , -- old format: only players as a dict
              Decode.dict Player.decoder
            ]
        )


playersDecoder : Decoder (Dict String Player)
playersDecoder =
    Decode.field "players" (Decode.list Player.decoder)
        |> Decode.map (List.map (\player -> ( player.name, player )))
        |> Decode.map Dict.fromList


encode : League -> Encode.Value
encode (League league) =
    Encode.object
        [ ( "players", Encode.list Player.encode (Dict.values league.players) )
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
    let
        ratings =
            Dict.values league.players
                |> List.map .rating
                |> List.sort

        medianishRating =
            ratings
                |> List.drop (List.length ratings // 2)
                |> List.head
                |> Maybe.withDefault Elo.initialRating
    in
    League { league | players = Dict.insert player.name (Player.setRating medianishRating player) league.players }


{-| -}
updatePlayer : Player -> League -> League
updatePlayer player (League league) =
    League { league | players = Dict.insert player.name player league.players }


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
                    Elo.win (kFactor won)
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
                    Elo.draw (kFactor (higherRankedPlayer playerA playerB))
                        { playerA = playerA.rating
                        , playerB = playerB.rating
                        }
            in
            league
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.playerA playerA))
                |> updatePlayer (Player.incrementMatchesPlayed (Player.setRating newRatings.playerB playerB))
                |> clearMatch


{-| -}
playInMatches : Int
playInMatches =
    5


{-| Thought: long-term, it may be better to say the 90th percentile of
competitors gets the insensitive k-factor instead of defining a cutoff.
-}
kFactor : Player -> Int
kFactor player =
    if player.matches < playInMatches then
        -- players who are new to the league should move around more so that
        -- they can get ranked closer to their actual correct position sooner.
        Elo.sensitiveKFactor * 2

    else if player.rating <= round (toFloat Elo.initialRating * 1.1) then
        -- players who have been around a while should still be able to easily
        -- move up in the rankings if it turns out they've been consistently
        -- underrated.
        --
        -- The threshold here may seem a little low here but it's only 4 of
        -- the 47 items in the list I use elo-anything for.
        Elo.sensitiveKFactor

    else
        -- players who are at the top of the ratings should be relatively
        -- stable.
        Elo.sensitiveKFactor // 2


{-| -}
higherRankedPlayer : Player -> Player -> Player
higherRankedPlayer a b =
    if a.rating > b.rating then
        a

    else
        b


{-| -}
clearMatch : League -> League
clearMatch (League league) =
    League { league | currentMatch = Nothing }
