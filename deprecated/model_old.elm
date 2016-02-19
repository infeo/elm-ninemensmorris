module Model where
import State exposing (..)
import Statey exposing (..)
import Board


--idee:
type alias Mill = (NodeId,NodeId,NodeId)       
        
--the Game & init of it
type alias Game =
    { pl1 : Player
    , pl2 : Player
    , plx : Player
    , currentPlayer : Player
    , gameMachine : StateMachine Player
    , status : Status
    , board : Graph String String
    }

initGame :Game
initGame =
    { pl1 = white,
    , pl2 = black,
    , plx = noOne,
    , gameMachine = millMachine
    , status = OnGoing
    , gameBoard = Board.board
    }

--the players and init of them
-- Annotation: the field myFields is in this case small. for larger games, a dictionary/associative array is suggested
type alias Player = 
    StateRecord { name: String, playing : Bool, numOfStonesInGame : Int
                , numOfStones: Int, hasMill : Bool, canMove : Bool, myFields : List NodeId}

white : Player
white = { name="White Hat", playing=True numOfStonesInGame=0, numOfStones=9, hasMill=False, canMove=True, myFields=[], state=initState}

black : Player
black = { white | name="Black Hat", playing=False}

noOne : Player
noOne = { white | name ="No Hat", playing=False, numOfStones=0, canMove=False, myFields=(nodeIds initGame.gameMachine)}
--the StateMachine and init of it    

--Create the States
initState : State
initState = makeState "init"

pseudoMoveState : State
pseudoMoveState = makeState "pseudoMove"

putState : State
putState = makeState "put"

slideState : State
slideState = makeState "slide"

jumpState : State
jumpState = makeState "jump"

pseudoTakeState : State
pseudoTakeState = makeState "pseudoTake"

hasMillState : State
hasMillState = makeState "hasMill"

pseudoEndTurnState : State
pseudoEndTurnState = makeState "pseudoEndTurn"

endState : State
endState = makeState "end"

--the StateMachine
millMachine :StateMachine Player
millMachine =
    { states =
        [ initState, pseudoMoveState, putState, slideState, jumpState, pseudoTakeState
        , hasMillState, checkState, endState]
    , transitions =
        [ ( initState, pseudoMoveState), (pseudoMoveState, putState), (pseudoMoveState, slideState)
        , (pseudoMoveState, jumpState), (putState,pseudoTakeState), (slideState,pseudoTakeState)
        , (jumpState, pseudoTakeState), (pseudoTakeState, hasMillState), (pseudoTakeState,checkState)
        , (hasMillState, checkState), (checkState,pseudoMoveState), (checkState, endState) ]
    , guards =
        [ {from = pseudoMoveState, to = putState, crit = \pl -> pl.numOfStonesInGame < 18 }
        , {from = pesudoMoveState, to = slideState, crit = \pl -> pl.numOfStones > 3 }
        , {from = pseudoMoveState, to = jumpState, crit = \pl -> pl.numOfStones = 3 }
        , {from = pseudoTakeState, to = checkState crit = \pl -> not pl.hasMill }
        , {from = pseudoTakeState, to = hasMillState, crit = \pl -> pl.hasMill }
        , {from = checkState, to = endState, crit = \pl -> pl.numOfStones < 3 || not pl.canMove } ]

--the status of the game        
type Status = OnGoing | End

-- for the board, see module board!
