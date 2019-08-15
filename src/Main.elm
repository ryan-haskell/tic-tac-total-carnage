module Main exposing (main)

import Browser
import Grid exposing (Grid, Location)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Player exposing (Player)


type alias Flags =
    ()


type alias Model =
    { player : Player
    , playingAreas : List Location
    , game : Grid Board
    }


type alias Board =
    Grid (Maybe Player)


main : Program Flags Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


init : Model
init =
    { player = Player.X
    , playingAreas = Grid.all
    , game = Grid.init (Grid.init Nothing)
    }



-- UPDATE


type Msg
    = PlacePiece
        { board : Location
        , cell : Location
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlacePiece locations ->
            let
                updatedGrid =
                    Grid.at locations.board model.game
                        |> Grid.update locations.cell (Just model.player)
                        |> (\game -> Grid.update locations.board game model.game)
            in
            { model
                | player = Player.other model.player
                , game = updatedGrid
                , playingAreas = playingArea locations.cell updatedGrid
            }


playingArea : Location -> Grid Board -> List Location
playingArea lastPlayedCellLocation grid =
    let
        destinationBoard =
            Grid.at lastPlayedCellLocation grid
    in
    if isBoardWon destinationBoard then
        Grid.all
            |> List.filterMap
                (\loc ->
                    if isBoardWon (Grid.at loc grid) then
                        Nothing

                    else
                        Just loc
                )

    else
        [ lastPlayedCellLocation ]


winnerOf : Board -> Maybe Player
winnerOf board =
    let
        hasThree player locations =
            List.length (List.filter (hasPlayer player) locations) == 3

        hasPlayer player location =
            Just player == Grid.at location board
    in
    Grid.lines
        |> List.filterMap
            (\lines ->
                if hasThree Player.X lines then
                    Just Player.X

                else if hasThree Player.O lines then
                    Just Player.O

                else
                    Nothing
            )
        |> List.head


isBoardWon : Board -> Bool
isBoardWon board =
    winnerOf board /= Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "game" ] <|
            List.concatMap
                (viewBoards model)
                (Grid.toListOfLists model.game)
        ]


viewBoards : Model -> List ( Location, Board ) -> List (Html Msg)
viewBoards model =
    List.map (viewBoard model)


viewBoard : Model -> ( Location, Board ) -> Html Msg
viewBoard model ( loc, board ) =
    case winnerOf board of
        Just player ->
            div [ class "game__board" ]
                [ text (Player.toString player) ]

        Nothing ->
            div [ class "game__board" ] <|
                List.concatMap
                    (viewCells model loc)
                    (Grid.toListOfLists board)


viewCells : Model -> Location -> List ( Location, Maybe Player ) -> List (Html Msg)
viewCells model boardLocation =
    List.map (viewCell model boardLocation)


viewCell : Model -> Location -> ( Location, Maybe Player ) -> Html Msg
viewCell model boardLocation ( cellLocation, value ) =
    let
        isDisabled =
            value /= Nothing || not (List.member boardLocation model.playingAreas)

        attr =
            if isDisabled then
                Attr.disabled True

            else
                Events.onClick
                    (PlacePiece
                        { board = boardLocation
                        , cell = cellLocation
                        }
                    )
    in
    button
        (attr
            :: [ class "game__cell"
               , Attr.classList
                    [ ( "game__cell--disabled", isDisabled )
                    ]
               ]
        )
        [ value
            |> Maybe.map Player.toString
            |> Maybe.withDefault ""
            |> text
        ]
