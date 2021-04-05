module Player exposing
    ( Player, init, htmlKey
    , PlayerId, id, idSorter
    , name
    , rating, setRating
    , matchesPlayed, setMatchesPlayedTestOnly, incrementMatchesPlayed
    , encode, decoder
    )

{-|

@docs Player, init, htmlKey

@docs PlayerId, id, idSorter

@docs name

@docs rating, setRating

@docs matchesPlayed, setMatchesPlayedTestOnly, incrementMatchesPlayed

@docs encode, decoder

-}

import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Murmur3
import Sort exposing (Sorter)


type Player
    = Player
        { id : PlayerId
        , name : String
        , rating : Int
        , matches : Int
        }


init : String -> Player
init name_ =
    Player
        { id = PlayerId (Murmur3.hashString 0 name_)
        , name = name_
        , rating = Elo.initialRating
        , matches = 0
        }


htmlKey : Player -> String
htmlKey (Player player) =
    let
        (PlayerId idInt) =
            player.id
    in
    String.fromInt idInt



-- ID


type PlayerId
    = PlayerId Int


id : Player -> PlayerId
id (Player player) =
    player.id


idSorter : Sorter PlayerId
idSorter =
    Sort.by (\(PlayerId id_) -> id_) Sort.increasing



-- NAME


name : Player -> String
name (Player player) =
    player.name



-- RATING


rating : Player -> Int
rating (Player player) =
    player.rating


setRating : Int -> Player -> Player
setRating rating_ (Player player) =
    Player { player | rating = max 0 rating_ }



-- MATCHES PLAYED


matchesPlayed : Player -> Int
matchesPlayed (Player player) =
    player.matches


setMatchesPlayedTestOnly : Int -> Player -> Player
setMatchesPlayedTestOnly matches (Player player) =
    Player { player | matches = matches }


incrementMatchesPlayed : Player -> Player
incrementMatchesPlayed (Player player) =
    Player { player | matches = player.matches + 1 }



-- INTEROP


decoder : Decoder Player
decoder =
    Decode.map4
        (\id_ name_ rating_ matches ->
            Player
                { id = id_
                , name = name_
                , rating = rating_
                , matches = matches
                }
        )
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
encode (Player player) =
    let
        (PlayerId idInt) =
            player.id
    in
    Encode.object
        [ ( "id", Encode.int idInt )
        , ( "name", Encode.string player.name )
        , ( "rating", Encode.int player.rating )
        , ( "matches", Encode.int player.matches )
        ]
