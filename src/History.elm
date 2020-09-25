module History exposing (History, current, goBack, init, mapPush, peekBack, push)

{-| -}


{-| A linear undo history. Basically works like a list zipper but drops
items older than the retention limit.
-}
type History a
    = History
        { retention : Int
        , past : List a
        , current : a
        , future : List a
        }


init : Int -> a -> History a
init retention initial =
    History
        { retention = retention
        , past = []
        , current = initial
        , future = []
        }


current : History a -> a
current (History guts) =
    guts.current


push : a -> History a -> History a
push a (History guts) =
    History
        { guts
            | past = List.take guts.retention (guts.current :: guts.past)
            , current = a
            , future = []
        }


mapPush : (a -> a) -> History a -> History a
mapPush fn history =
    push (fn (current history)) history


peekBack : History a -> Maybe a
peekBack (History guts) =
    List.head guts.past


goBack : History a -> Maybe (History a)
goBack (History guts) =
    case guts.past of
        mostRecent :: rest ->
            (Just << History)
                { guts
                    | past = rest
                    , current = mostRecent
                    , future = guts.current :: guts.future
                }

        [] ->
            Nothing
