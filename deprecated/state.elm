module State where

import Statey exposing (..)

type alias Player = 
    StateRecord { name: String, allStonesinField : Boolean
                , numOfStones: Int, hasMill : Boolean, canMove : Boolean }

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
stateMachine :StateMachine Player
stateMachine =
    { states =
        [ initState, pseudoMoveState, putState, slideState, jumpState, pseudoTakeState
        , hasMillState, pseudoEndTurnState, endState]
    , transitions =
        [ ( initState, pseudoMoveState), (pseudoMoveState, putState), (pseudoMoveState, slideState)
        , (pseudoMoveState, jumpState), (putState,pseudoTakeState), (slideState,pseudoTakeState)
        , (jumpState, pseudoTakeState), (pseudoTakeState, hasMillState), (pseudoTakeState,pseudoEndTurnState)
        , (hasMillState, pseudoEndTurnState), (pseudoEndTurnState,pseudoMoveState), (pseudoEndTurnState, endState) ]
    , guards =
        [ {from = pseudoMoveState, to = putState, crit = \pl -> not pl.allStonesInField }
        , {from = pesudoMoveState, to = slideState, crit = \pl -> pl.numOfStones > 3 }
        , {from = pseudoMoveState, to = jumpState, crit = \pl -> pl.numOfStones = 3 }
        , {from = pseudoTakeState, to = pseudoEndTurnState crit = \pl -> not pl.hasMill }
        , {from = pseudoTakeState, to = hasMillState, crit = \pl -> pl.hasMill }
        , {from = pseudoEndTurnState, to = endState, crit = \pl -> pl.numOfStones <3 || not pl.canMove } ]
        
--the players
white : Player
white = { name="White Hat", allStonesInField=False, numOfStones=9, hasMill=False, canMove=True, state=initState}

black : Player
black = { white | name="Black Hat"}

--changing State & Stats

--WARNING: this funtion is dependable on?of? the order of the states
trans pl = case head << automatedStateSelect <| stateMachine pl of
            Just tuple -> snd tuple
            Nothing -> pl
                
            
            
realChange =

{-
--only works Because the statemachine is small
--dammn strictness
--ok, what does this funktion do?
--take a statemachine and a record and output all possible States in a list
-}
automatedStateSelect : StateMachine a -> StateRecord a -> List State
automatedStateSelect m r =  let checkTran = \s2 -> case transition m s2 r of
                                                    Ok val  -> Just (s2, val)
                                                    Err err -> Nothing
                            in
                            filterMap checkTran (getStates m)
