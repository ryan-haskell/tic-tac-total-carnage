module Grid exposing
    ( Grid
    , Location
    , all
    , at
    , every
    , init
    , lines
    , toListOfLists
    , update
    )


type Grid a
    = Grid (Details a)


type alias Details a =
    { topLeft : a
    , topCenter : a
    , topRight : a
    , middleLeft : a
    , middleCenter : a
    , middleRight : a
    , bottomLeft : a
    , bottomCenter : a
    , bottomRight : a
    }



-- LOCATIONS


type Location
    = Location ( Vertical, Horizontal )


type Vertical
    = Top
    | Middle
    | Bottom


verticals : List Vertical
verticals =
    [ Top, Middle, Bottom ]


type Horizontal
    = Left
    | Center
    | Right


horizontals : List Horizontal
horizontals =
    [ Left, Center, Right ]


all : List Location
all =
    verticals
        |> List.concatMap (\v -> List.map (\h -> Location ( v, h )) horizontals)


lines : List (List Location)
lines =
    [ [ topLeft, topCenter, topRight ]
    , [ middleLeft, middleCenter, middleRight ]
    , [ bottomLeft, bottomCenter, bottomRight ]
    , [ topLeft, middleLeft, bottomLeft ]
    , [ topCenter, middleCenter, bottomCenter ]
    , [ topRight, middleRight, bottomRight ]
    , [ topLeft, middleCenter, bottomRight ]
    , [ topRight, middleCenter, bottomLeft ]
    ]


topLeft : Location
topLeft =
    Location ( Top, Left )


topCenter : Location
topCenter =
    Location ( Top, Center )


topRight : Location
topRight =
    Location ( Top, Right )


middleLeft : Location
middleLeft =
    Location ( Middle, Left )


middleCenter : Location
middleCenter =
    Location ( Middle, Center )


middleRight : Location
middleRight =
    Location ( Middle, Right )


bottomLeft : Location
bottomLeft =
    Location ( Bottom, Left )


bottomCenter : Location
bottomCenter =
    Location ( Bottom, Center )


bottomRight : Location
bottomRight =
    Location ( Bottom, Right )



-- GRID


init : a -> Grid a
init value =
    Grid <|
        Details
            value
            value
            value
            value
            value
            value
            value
            value
            value


at : Location -> Grid a -> a
at (Location location) (Grid grid) =
    case location of
        ( Top, Left ) ->
            grid.topLeft

        ( Top, Center ) ->
            grid.topCenter

        ( Top, Right ) ->
            grid.topRight

        ( Middle, Left ) ->
            grid.middleLeft

        ( Middle, Center ) ->
            grid.middleCenter

        ( Middle, Right ) ->
            grid.middleRight

        ( Bottom, Left ) ->
            grid.bottomLeft

        ( Bottom, Center ) ->
            grid.bottomCenter

        ( Bottom, Right ) ->
            grid.bottomRight


every : (a -> Bool) -> Grid a -> Bool
every predicate grid =
    List.all (\loc -> predicate (at loc grid)) all


update : Location -> a -> Grid a -> Grid a
update (Location location) value (Grid grid) =
    Grid <|
        case location of
            ( Top, Left ) ->
                { grid | topLeft = value }

            ( Top, Center ) ->
                { grid | topCenter = value }

            ( Top, Right ) ->
                { grid | topRight = value }

            ( Middle, Left ) ->
                { grid | middleLeft = value }

            ( Middle, Center ) ->
                { grid | middleCenter = value }

            ( Middle, Right ) ->
                { grid | middleRight = value }

            ( Bottom, Left ) ->
                { grid | bottomLeft = value }

            ( Bottom, Center ) ->
                { grid | bottomCenter = value }

            ( Bottom, Right ) ->
                { grid | bottomRight = value }


toListOfLists : Grid a -> List (List ( Location, a ))
toListOfLists (Grid grid) =
    [ [ ( topLeft, grid.topLeft )
      , ( topCenter, grid.topCenter )
      , ( topRight, grid.topRight )
      ]
    , [ ( middleLeft, grid.middleLeft )
      , ( middleCenter, grid.middleCenter )
      , ( middleRight, grid.middleRight )
      ]
    , [ ( bottomLeft, grid.bottomLeft )
      , ( bottomCenter, grid.bottomCenter )
      , ( bottomRight, grid.bottomRight )
      ]
    ]
