module History exposing (History, current, goBack, goForward, init, mapInPlace, mapPush, peekBack, peekForward, push)

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


mapInPlace : (a -> a) -> History a -> History a
mapInPlace fn (History guts) =
    History { guts | current = fn guts.current }


peekBack : History a -> Maybe a
peekBack (History guts) =
    List.head guts.past


peekForward : History a -> Maybe a
peekForward (History guts) =
    List.head guts.future


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


goForward : History a -> Maybe (History a)
goForward (History guts) =
    case guts.future of
        nextRecent :: rest ->
            (Just << History)
                { guts
                    | past = guts.current :: guts.past
                    , current = nextRecent
                    , future = rest
                }

        [] ->
            Nothing
