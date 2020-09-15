module EloTest exposing (..)

import Elo exposing (..)
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
                odds rating rating
                    |> Expect.within (Relative 0.001) 0.5
        , fuzz inequalRatingsFuzzer "given inequal ratings, the outcome favors the higher of the two ratings" <|
            \( a, b ) ->
                odds (max a b) (min a b)
                    |> Expect.greaterThan 0.5
        ]


newRating : Test
newRating =
    describe "newRating"
        [ fuzz ratingFuzzer "a player gains more points by winning against a heavy favorite than a relative equal" <|
            \player ->
                let
                    favorite =
                        player + 1000

                    againstRelativeEqual =
                        Elo.win Elo.sensitiveKFactor
                            { won = player
                            , lost = player + 100
                            }

                    againstFavorite =
                        Elo.win Elo.sensitiveKFactor
                            { won = player
                            , lost = player + 1000
                            }
                in
                againstRelativeEqual.won |> Expect.lessThan againstFavorite.won
        , fuzz2 ratingFuzzer (Fuzz.intRange 1 32) "a higher k-value produces a larger score difference than a lower one" <|
            \player kFactorIncrease ->
                let
                    matchup =
                        { won = player
                        , lost = player + 100
                        }

                    low =
                        Elo.win Elo.sensitiveKFactor matchup

                    high =
                        Elo.win (Elo.sensitiveKFactor + kFactorIncrease) matchup
                in
                low.won |> Expect.lessThan high.won
        ]
