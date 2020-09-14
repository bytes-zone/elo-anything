module League exposing
    ( League, init, decoder, encode
    , players, addPlayer, updatePlayer, retirePlayer
    , Match(..), currentMatch, nextMatch, startMatch, finishMatch
    )

{-|

@docs League, init, decoder, encode

@docs players, addPlayer, updatePlayer, retirePlayer

@docs Match, currentMatch, nextMatch, startMatch, finishMatch

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Player exposing (Player)
import Random exposing (Generator)


type League
    = League
        { players : Dict String Player
        , currentMatch : Maybe Match
        }


type Match
    = MatchBetween Player Player



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
        (\newPlayers -> League { players = newPlayers, currentMatch = Nothing })
        (Decode.oneOf
            [ Decode.field "players" (Decode.list Player.decoder)
                |> Decode.map (List.map (\player -> ( player.name, player )))
                |> Decode.map Dict.fromList
            , -- old formats
              Decode.dict Player.decoder
            ]
        )


encode : League -> Encode.Value
encode (League league) =
    Encode.object
        [ ( "players", Encode.list Player.encode (Dict.values league.players) ) ]



-- PLAYERS


players : League -> List Player
players (League league) =
    Dict.values league.players


addPlayer : Player -> League -> League
addPlayer player (League league) =
    League { league | players = Dict.insert player.name player league.players }


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

                    Just (MatchBetween a b) ->
                        if player.name == a.name || player.name == b.name then
                            Nothing

                        else
                            league.currentMatch
        }



-- MATCHES


currentMatch : League -> Maybe Match
currentMatch (League league) =
    league.currentMatch


nextMatch : League -> Generator (Maybe Match)
nextMatch (League league) =
    let
        allPlayers =
            Dict.values league.players

        minimumMatches =
            allPlayers
                |> List.map .matches
                |> List.minimum
                |> Maybe.withDefault 0

        leastPlayed =
            allPlayers
                |> List.filter (\player -> player.matches == minimumMatches)
    in
    case allPlayers of
        a :: b :: rest ->
            allPlayers
                |> List.Extra.uniquePairs
                |> List.filter (\( left, right ) -> List.member left leastPlayed || List.member right leastPlayed)
                |> List.map
                    (\( left, right ) ->
                        ( toFloat <| abs (left.rating - right.rating)
                        , ( left, right )
                        )
                    )
                |> -- flip the ordering so that the smallest gap / match adjustment is the most
                   -- likely to be picked.
                   (\weights ->
                        let
                            maxDiff =
                                List.maximum (List.map Tuple.first weights) |> Maybe.withDefault (10 ^ 9)
                        in
                        List.map (\( diff, pair ) -> ( (maxDiff - diff) ^ 2, pair )) weights
                   )
                |> (\weights ->
                        case weights of
                            firstWeight :: restOfWeights ->
                                Random.weighted firstWeight restOfWeights

                            _ ->
                                -- how did we get here? Unless... a and b were the same
                                -- player? Sneaky caller!
                                Random.constant ( a, b )
                   )
                |> Random.map (\( left, right ) -> MatchBetween left right)
                |> Random.map Just

        _ ->
            Random.constant Nothing


startMatch : Match -> League -> League
startMatch match (League league) =
    League { league | currentMatch = Just match }


finishMatch : League -> League
finishMatch (League league) =
    League { league | currentMatch = Nothing }
