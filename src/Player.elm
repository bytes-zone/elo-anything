module Player exposing (Player, decoder, encode, incrementMatchesPlayed, init, setRating)

import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Murmur3


type alias Player =
    { id : Int
    , name : String
    , rating : Int
    , matches : Int
    }


init : String -> Player
init name =
    { id = Murmur3.hashString 0 name
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
        )
        (Decode.field "name" Decode.string)
        (Decode.field "rating" Decode.int)
        (Decode.field "matches" Decode.int)


encode : Player -> Value
encode { id, name, rating, matches } =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "name", Encode.string name )
        , ( "rating", Encode.int rating )
        , ( "matches", Encode.int matches )
        ]
