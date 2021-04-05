module Player exposing (Player, PlayerId, decoder, encode, incrementMatchesPlayed, init, playerIdFromIntForTestOnly, setRating)

import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Murmur3


type PlayerId
    = PlayerId Int


playerIdFromIntForTestOnly : Int -> PlayerId
playerIdFromIntForTestOnly =
    PlayerId


type alias Player =
    { id : PlayerId
    , name : String
    , rating : Int
    , matches : Int
    }


init : String -> Player
init name =
    { id = PlayerId (Murmur3.hashString 0 name)
    , name = name
    , rating = Elo.initialRating
    , matches = 0
    }


setRating : Int -> Player -> Player
setRating rating player =
    { player | rating = rating }


incrementMatchesPlayed : Player -> Player
incrementMatchesPlayed player =
    { player | matches = player.matches + 1 }


decoder : Decoder Player
decoder =
    Decode.map4 Player
        (Decode.oneOf
            [ Decode.field "id" Decode.int
            , Decode.field "name" Decode.string
                |> Decode.map (Murmur3.hashString 0)
            ]
            |> Decode.map PlayerId
        )
        (Decode.field "name" Decode.string)
        (Decode.field "rating" Decode.int)
        (Decode.field "matches" Decode.int)


encode : Player -> Value
encode { id, name, rating, matches } =
    let
        (PlayerId idInt) =
            id
    in
    Encode.object
        [ ( "id", Encode.int idInt )
        , ( "name", Encode.string name )
        , ( "rating", Encode.int rating )
        , ( "matches", Encode.int matches )
        ]
