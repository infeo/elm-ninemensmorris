module View (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color
import Array
import List
import Model exposing (..)
import Update


display : Game -> Element
display g =
  case g.status of
    OnGoing ->
      displayOngoing g

    Draw ->
      displayDraw

    _ ->
      displayWin g


displayOngoing : Game -> Element
displayOngoing g =
  let
    pl1 =
      g.pl1

    pl2 =
      g.pl2

    view =
      g.view

    curr =
      if pl1.playing then
        pl1
      else
        pl2

    whiteStones =
      List.filterMap (\x -> Array.get x view.nodeToCoord) pl1.myFields

    blackStones =
      List.filterMap (\x -> Array.get x view.nodeToCoord) pl2.myFields

    showStones =
      \color ls -> group <| List.map (\( x, y ) -> move ( toFloat x, toFloat (-y) ) (filled color (circle 10))) ls
  in
    container 600 600 middle
      <| collage
          500
          500
          [ realField
          , showPlayer pl1 |> move ( -100, 175 )
          , showPlayer pl2 |> move ( 100, 175 )
          , toForm (show g.status)
          , showStones Color.white whiteStones
          , showStones Color.black blackStones
          ]


displayWin : Game -> Element
displayWin g =
  let
    winner =
      if g.pl1.state == endState then
        "Player 2 wins"
      else
        "Player 1 wins"
  in
    container 600 600 middle
      <| collage
          500
          500
          [ toForm (show winner) ]


displayDraw : Element
displayDraw =
  container 600 600 middle
    <| collage
        500
        500
        [ toForm (show "Draw!") ]


realField : Form
realField =
  group
    [ filled Color.grey (square 400)
    , outlined defaultLine (square 300.0)
      -- 50 * 3 = 150
    , outlined defaultLine (square 195.0)
      -- 32.5 *3 = 90+6+1.5 = 97.5
    , outlined defaultLine (square 90.0)
      -- 15 * 3 = 30+15= 45
    , traced defaultLine (segment ( -150, 0 ) ( -45, 0 ))
    , traced defaultLine (segment ( 45, 0 ) ( 150, 0 ))
    , traced defaultLine (segment ( 0, 45 ) ( 0, 150 ))
    , traced defaultLine (segment ( 0, -150 ) ( 0, -45 ))
    ]


showPlayer : Player -> Form
showPlayer pl =
  let
    ma =
      toForm << show
  in
    group
      [ ma pl.name
      , ma pl.playing |> move ( 0, 10 )
      , ma pl.numOfStones |> move ( 0, 20 )
      , ma pl.numOfStonesInGame |> move ( 0, 30 )
      , ma pl.closedMill |> move ( 0, 40 )
      , ma pl.canMove |> move ( 0, 50 )
      , ma pl.state |> move ( 0, 60 )
      , ma pl.mills |> move ( 0, 70 )
      ]


main : Signal.Signal Element
main =
  Signal.map display Update.gameState
