module Elo exposing (Outcome(..), initialRating, newRating, sensitiveKFactor)

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


type Outcome
    = WonAgainst
    | DrewWith
    | LostTo


invertOutcome : Outcome -> Outcome
invertOutcome outcome =
    case outcome of
        WonAgainst ->
            LostTo

        DrewWith ->
            DrewWith

        LostTo ->
            WonAgainst


score : Outcome -> Float
score outcome =
    case outcome of
        WonAgainst ->
            1

        DrewWith ->
            0.5

        LostTo ->
            0


{-| Both player's new ratings given the outcome of the match.

For example, to say player a won against player b:

    newRating sensitiveKFactor playerARating WonAgainst playerBRating

-}
newRating : Int -> Int -> Outcome -> Int -> ( Int, Int )
newRating kFactor a outcome b =
    ( toFloat a + toFloat kFactor * (score outcome - odds a b) |> round
    , toFloat b + toFloat kFactor * (score (invertOutcome outcome) - odds b a) |> round
    )
