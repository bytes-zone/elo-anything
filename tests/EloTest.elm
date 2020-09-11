module EloTest exposing (..)

import Elo
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Test exposing (..)


ratingFuzzer : Fuzzer Int
ratingFuzzer =
    Fuzz.intRange
        (Elo.initialRating - 500)
        (Elo.initialRating + 500)


inequalRatingsFuzzer : Fuzzer ( Int, Int )
inequalRatingsFuzzer =
    Fuzz.map2
        (\a b ->
            if a == b then
                ( a, b + 1 )

            else
                ( a, b )
        )
        ratingFuzzer
        ratingFuzzer


oddsTest : Test
oddsTest =
    describe "odds"
        [ fuzz ratingFuzzer "given equal ratings, the outcome is 50/50" <|
            \rating ->
                Elo.odds rating rating
                    |> Expect.within (Relative 0.001) 0.5
        , fuzz inequalRatingsFuzzer "given inequal ratings, the outcome favors the higher of the two ratings" <|
            \( a, b ) ->
                Elo.odds (max a b) (min a b)
                    |> Expect.greaterThan 0.5
        ]
