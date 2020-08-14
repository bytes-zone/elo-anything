module Player exposing (Player, decoder, encode, incrementMatchesPlayed, init, setRating)

import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Player =
    { name : String
    , rating : Int
    , matches : Int
    }


init : String -> Player
init name =
    { name = name
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
    Decode.map3 Player
        (Decode.field "name" Decode.string)
        (Decode.field "rating" Decode.int)
        (Decode.field "matches" Decode.int)


encode : Player -> Value
encode { name, rating, matches } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "rating", Encode.int rating )
        , ( "matches", Encode.int matches )
        ]
