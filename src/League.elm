module League exposing (League, addNewPlayer, decoder, encode, init, retirePlayer, updatePlayer)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Player exposing (Player)


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
