module Player exposing
    ( Player(..)
    , other
    , toString
    )


type Player
    = X
    | O


other : Player -> Player
other player =
    case player of
        X ->
            O

        O ->
            X


toString : Player -> String
toString player =
    case player of
        X ->
            "X"

        O ->
            "O"
