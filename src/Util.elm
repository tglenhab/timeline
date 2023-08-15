module Util exposing (..)




listRemove : a -> List a -> List a
listRemove a l =
    List.filter (\ x -> x /= a) l

listInsert : a -> List a -> List a
listInsert a l =
    a :: l

listInterleave : List a -> List a -> List a
listInterleave l1 l2 =
    case l1 of
        [] -> l2
        head1 :: tail1 ->
            case l2 of
                [] -> l1
                head2 :: tail2 ->
                    head1 :: head2 :: listInterleave tail1 tail2


guessCorrect : Int -> Maybe Int -> Maybe Int -> Bool
guessCorrect guess d1 d2 =
    Maybe.withDefault True (Maybe.map ((>=) guess) d1)
        && Maybe.withDefault True (Maybe.map ((<=) guess) d2)


betweenStr : Maybe Int -> Maybe Int -> String
betweenStr d1 d2 =
    case (d1, d2) of
        (Nothing, Nothing) -> ""
        (Just i, Nothing) -> "after " ++ String.fromInt i
        (Nothing, Just i) -> "before " ++ String.fromInt i
        (Just i, Just j) -> "between " ++ String.fromInt i ++ " and " ++ String.fromInt j

