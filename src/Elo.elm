module Elo exposing (draw, initialRating, odds, sensitiveKFactor, win)

{-| Calculate [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) scores.
-}


{-| The initial rating can be whatever you want it to be, but somewhere
between 1,000 and 1,500 seems to be pretty normal. This sets initial ratings
at 1200.
-}
initialRating : Int
initialRating =
    1200


{-| A sensitive K-factor. This is good for if you have relatively few matches
(like American Football) because score adjustments will be larger per game
played. If you notice scores jumping around a lot, lessen this.

You may also want to consider using a staggered K-factor to adjust
for new players so as to find a stable rating quicker, as in chess (see
[Wikipedia](https://en.wikipedia.org/wiki/Elo_rating_system#Most_accurate_K-factor)
for more on this approach.)

-}
sensitiveKFactor : Int
sensitiveKFactor =
    32


{-| The odds that a will beat b

To flip it, `1 - (odds a b)` or just flip the arguments.

-}
odds : Int -> Int -> Float
odds a b =
    let
        rA =
            10 ^ (toFloat a / 400)

        rB =
            10 ^ (toFloat b / 400)
    in
    rA / (rA + rB)


{-| One player won, the other player lost.
-}
win : Int -> { won : Int, lost : Int } -> { won : Int, lost : Int }
win kFactor { won, lost } =
    { won = toFloat won + toFloat kFactor * (1 - odds won lost) |> round
    , lost = toFloat lost + toFloat kFactor * (0 - odds lost won) |> round
    }


{-| The players drew/tied.
-}
draw : Int -> { playerA : Int, playerB : Int } -> { playerA : Int, playerB : Int }
draw kFactor { playerA, playerB } =
    { playerA = toFloat playerA + toFloat kFactor * (0.5 - odds playerA playerB) |> round
    , playerB = toFloat playerB + toFloat kFactor * (0.5 - odds playerB playerA) |> round
    }
