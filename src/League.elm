module League exposing (League, addNewPlayer, decoder, encode, init, match, retirePlayer, updatePlayer)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Player exposing (Player)
import Random exposing (Generator)


type alias League =
    { players : Dict String Player
    }


init : League
init =
    { players = Dict.empty }


addNewPlayer : Player -> League -> League
addNewPlayer player league =
    { league | players = Dict.insert player.name player league.players }


retirePlayer : Player -> League -> League
retirePlayer player league =
    { league | players = Dict.remove player.name league.players }


updatePlayer : Player -> League -> League
updatePlayer =
    addNewPlayer


decoder : Decoder League
decoder =
    Decode.oneOf
        [ Decode.field "players" (Decode.list Player.decoder)
            |> Decode.map (List.map (\player -> ( player.name, player )))
            |> Decode.map Dict.fromList
            |> Decode.map League
        , -- old formats
          Decode.map League (Decode.dict Player.decoder)
        ]


encode : League -> Encode.Value
encode league =
    Encode.object
        [ ( "players", Encode.list Player.encode (Dict.values league.players) ) ]


{-| We need at least two players to guarantee that we return two distinct
players.
-}
match : League -> Generator (Maybe ( Player, Player ))
match league =
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
                                    |> Random.map Just

                            _ ->
                                -- how did we get here? Unless... a and b were the same
                                -- player? Sneaky caller!
                                Random.constant (Just ( a, b ))
                   )

        _ ->
            Random.constant Nothing
