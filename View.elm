module View (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Color
import Array
import List
import Model exposing (..)
import Update
import Maybe
import Inputs


display : Game -> Element
display g =
  case g.status of
    OnGoing ->
      displayOngoing g

    Draw ->
      displayDraw

    Win ->
      displayWin g

    _ ->
      displayInit


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

    myLineStyle =
      let
        tmp =
          solid Color.red
      in
        { tmp | width = 5.0 }

    markedStone =
      case g.fstNod `Maybe.andThen` (\id -> Array.get id view.nodeToCoord) of
        Just ( x, y ) ->
          move ( toFloat x, toFloat (-y) ) <| outlined (myLineStyle) <| circle 10

        Nothing ->
          filled (Color.hsla (degrees 0) 1 0.5 0) <| circle 10

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
          [ view.concreteRep
          , showPlayer pl1 |> move ( -100, 175 )
          , showPlayer pl2 |> move ( 100, 175 )
            --, toForm (show g.status)
          , markedStone
          , showStones Color.white whiteStones
          , showStones Color.black blackStones
          ]


displayDraw : Element
displayDraw =
  container 600 600 middle
    <| collage
        500
        500
        [ toForm (show "Draw!") |> move ( 0, 20 )
        , replayBut |> move ( 0, -20 )
        ]


displayWin : Game -> Element
displayWin g =
  let
    winner =
      case wrapper' g.pl1 of
        End ->
          "Player 2 wins"

        _ ->
          "Player 1 wins"
  in
    container 600 600 middle
      <| collage
          500
          500
          [ toForm (show winner) |> move ( 0, 20 )
          , replayBut |> move ( 0, -20 )
          ]


displayInit : Element
displayInit =
  container 600 600 middle
    <| collage
        500
        500
        [ startBut |> move (0,20)
        , toForm selectBoardDropDown |> move (0, -20)
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


startBut : Form
startBut =
  toForm <| button (Signal.message (buttonBox.address) False) "Start"


replayBut : Form
replayBut =
  toForm <| button (Signal.message (buttonBox.address) True) "Replay?"


buttonBox : Signal.Mailbox Bool
buttonBox =
  Signal.mailbox True

selectBox : Signal.Mailbox (Maybe Board)
selectBox = Signal.mailbox Nothing

selectBoardDropDown : Element
selectBoardDropDown =
    dropDown (Signal.message selectBox.address)
        [ ("NineMensMorris", Just Nine)
        , ("RoundMensMorris", Just Model.Round)
        , ("ThreeMensMorris", Just Three)
        , ("FiveMensMorris", Just Five)
        , ("SixMensMorris", Just Six)
        , ("SevenMensMorris", Just Seven)
        ]
        
                            
gui : Signal.Signal Game
gui =
  Signal.foldp reset initGame <| Signal.map3(,,) Inputs.input buttonBox.signal selectBox.signal


reset : ( ( Int, Int ), Bool, Maybe.Maybe Board ) -> Game -> Game
reset ( click, replay, mybBo ) g =
  case mybBo of
    Nothing -> initGame 
    _ -> if replay then
            initGame
         else
            Update.stepGame click g -- für die KI: Update.kI click g


main : Signal.Signal Element
main =
  Signal.map display gui
