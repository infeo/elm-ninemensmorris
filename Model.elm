module Model (..) where

import Statey exposing (..)
import Board
import Graph exposing (..)
import Result
import Maybe


type alias Mill =
  ( NodeId, NodeId, NodeId )

type alias BoardGraph = Graph (Int,Int,Int) String

--the Game & init of it


type alias Game =
  { pl1 : Player
  , pl2 : Player
  , plx : Player
  , machine : StateMachine Player
  , status : Status
  , view : Board.View
  , fstNod : Maybe NodeId
  }


initGame : Game
initGame =
  { pl1 = white
  , pl2 = black
  , plx = noOne
  , machine = millMachine
  , status = Init
  , view = Board.realView
  , fstNod = Nothing
  }



--the players and init of them
-- Annotation: the field myFields is in this case small. for larger games, a dictionary/associative array is suggested


type alias Player =
  StateRecord
    { name : String
    , playing : Bool
    , numOfStonesInGame : Int
    , numOfStones : Int
    , closedMill : Bool
    , mills : List Mill
    , canMove : Bool
    , myFields : List NodeId
    , ty : FakePlayer
    }


white : Player
white =
  { name = "White Hat", playing = True, numOfStonesInGame = 0, numOfStones = 9, closedMill = False, mills = [], canMove = True, myFields = [], ty = White, state = pseudoMoveState }


black : Player
black =
  { white | name = "Black Hat", playing = False, ty = Black }


noOne : Player
noOne =
  { white | name = "No Hat", playing = False, numOfStones = 0, canMove = False, myFields = Board.nodeList, ty = NoOne }


type FakePlayer
  = White
  | Black
  | NoOne



--the StateMachine and init of it
--Create the States


pseudoMoveState : State
pseudoMoveState =
  makeState "pseudoMove"


putState : State
putState =
  makeState "put"


slideState : State
slideState =
  makeState "slide"


jumpState : State
jumpState =
  makeState "jump"


pseudoTakeState : State
pseudoTakeState =
  makeState "pseudoTake"


hasMillState : State
hasMillState =
  makeState "hasMill"


checkState : State
checkState =
  makeState "checkState"


endState : State
endState =
  makeState "end"



--the StateMachine


millMachine : StateMachine Player
millMachine =
  { states =
      [ pseudoMoveState
      , putState
      , slideState
      , jumpState
      , pseudoTakeState
      , hasMillState
      , checkState
      , endState
      ]
  , transitions =
      [ ( pseudoMoveState, putState )
      , ( pseudoMoveState, slideState )
      , ( pseudoMoveState, jumpState )
      , ( putState, pseudoTakeState )
      , ( slideState, pseudoTakeState )
      , ( jumpState, pseudoTakeState )
      , ( pseudoTakeState, hasMillState )
      , ( pseudoTakeState, checkState )
      , ( hasMillState, checkState )
      , ( checkState, pseudoMoveState )
      , ( checkState, endState )
      ]
  , guards =
      [ { from = pseudoMoveState, to = putState, fn = \pl -> pl.numOfStonesInGame < 9 }
      , { from = pseudoMoveState, to = slideState, fn = \pl -> pl.numOfStones > 3 && pl.numOfStonesInGame == 9 }
      , { from = pseudoMoveState, to = jumpState, fn = \pl -> pl.numOfStones == 3 && pl.numOfStonesInGame == 9 }
      , { from = pseudoTakeState, to = checkState, fn = \pl -> not pl.closedMill }
      , { from = pseudoTakeState, to = hasMillState, fn = \pl -> pl.closedMill }
      , { from = checkState, to = endState, fn = \pl -> pl.numOfStonesInGame == 9 && (pl.numOfStones < 3 || not pl.canMove) }
      , { from = checkState, to = pseudoMoveState, fn = \pl -> pl.numOfStonesInGame < 9 || (pl.numOfStones >= 3 && pl.canMove) }
      ]
  }



--the status of the game


type Status
  = Init
  | OnGoing
  | Win
  | Draw



-- for the board, see module board!
-- some helpful functions
{-
   --only works Because the statemachine is small
   --dammn strictness
   --take a statemachine and a record and output all possible States in a list
-}


automatedStateSelect : StateMachine Player -> Player -> List Player
automatedStateSelect m pl =
  let
    checkTransition =
      \st ->
        Result.toMaybe (transition m st pl)
  in
    List.filterMap checkTransition (getStates m)



-- next transition for a player
--trans : Player -> StateMachine Player -> Player


trans : Player -> StateMachine Player -> Player
trans pl m =
  let
    stLs =
      automatedStateSelect m pl
  in
    Maybe.withDefault pl (List.head stLs)


wrapper : { a | state : State } -> State
wrapper x =
  getState x


wrapper' : { a | state : State } -> FakeState
wrapper' x =
  hide << getState <| x


type FakeState
  = PseudoMove
  | Put
  | Slide
  | Jump
  | PseudoTake
  | HasMill
  | Check
  | End


hide : State -> FakeState
hide st =
  if st == pseudoMoveState then
    PseudoMove
  else if st == putState then
    Put
  else if st == slideState then
    Slide
  else if st == jumpState then
    Jump
  else if st == pseudoTakeState then
    PseudoTake
  else if st == hasMillState then
    HasMill
  else if st == checkState then
    Check
  else
    End


type Board = Round | Three | Five | Six | Seven | Nine
