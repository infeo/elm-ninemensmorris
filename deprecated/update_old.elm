module Update where

import Model exposing (Game, initGame)
import Inputs exposing (..)
import Graph
import Statey


{-
--ToDo:
implement doomfunc
-}

--for easy use
wizard = initGame.gameMachine
magic = initGame.gameBoard

opponent : Player -> Player
opponent pl = case pl of
                black -> white
                white -> black

isInMill : Player -> (NodeId -> Bool)
isInMill pl = let ls = 
                in (\id -> member id ls)

validTurns : (Player,Player,Player) -> NodeId -> List NodeId
validTurns (curr,pass,dummy) n = 
    let st = getState curr
        freeNodes = dummy.myFields
    in case st of
        jumpState -> freeNodes
        putState -> freeNodes
        slideState -> let x = get id magic
                      in case x of
                            Just ctx -> filter (\x-> member x freeNodes) (alongOutgoingEdges ctx) 
                            Nothing ->  []
        hasMillState -> filter (isInMill pass) pass.myFields -- toDo: function isInMill
        checkState -> let ls = concatMap helper2 curr.myFields,
                          ls2= filter (\x-> not (member x pl.myFields)) ls,
                          ls3 = filter (\x -> member x freeNodes) ls2
                      in ls3    

helper2 : NodeId -> List NodeId
helper2 id = case (get id magic) of
                Just(ctx) -> alongOutgoingEdges ctx 
                Nothing -> []

--currently it works only with magic!
checkForMill : Player -> NodeId -> Bool
checkForMill pl nod =   let ls = map helper (pl.myfields),
                            (a,b,c) = helper nod
                            aBool = case a of
                                    0 -> member (1,b,c) ls && member (2,b,c) ls
                                    1 -> member (0,b,c) ls && member (2,b,c) ls 
                                    2 -> member (1,b,c) ls && member (0,b,c) ls
                            bBool = case b of
                                    0 -> member (a,1,c) ls && member (a,2,c) ls
                                    1 -> member (a,0,c) ls && member (a,2,c) ls 
                                    2 -> member (a,1,c) ls && member (a,0,c) ls
                            cBool = case c of
                                    0 -> member (a,b,1) ls && member (a,b,2) ls
                                    1 -> member (a,b,0) ls && member (a,b,2) ls 
                                    2 -> member (a,b,1) ls && member (a,b,0) ls
                        in (aBool && (b=1 || c=1)) || bBool || cBool
                        
helper : NodeId -> (Int,Int,Int)
helper x = case (get x magic) of
            Just (ctx) -> ctx.node.label
            Nothing -> (0,0,0) -- impossible to get here, but i'm supposed to write something

--Step the active Player forward
stepPlayer : Player -> NodeId -> Player
stepPlayer pl nod = let newPl = 
                            case (getState pl) of
                            putState -> 
                                { pl | 
                                    numOfStonesInGame = pl.numOfStonesInGame + 1,
                                    hasMill = checkForMill pl nod,
                                    myFields = nod::pl.myFields 
                                }
                            slideState -> 
                                { pl |
                                    hasMill = checkForMill pl nod,
                                    myFields = nod::pl.myFields
                                }
                            jumpState -> 
                                { pl |
                                    hasMill = checkForMill pl nod,
                                    myFields = nod::pl.myFields
                                }
                            hasMill -> {pl | hasMill = False}
                            _ -> pl
                    in {newPl | state =trans newPl wizard} 
 
{-
Always the same tactic:
1. Update the current Player
2. Update the Passive Player
3. Update the Dummy Player
-} 
stepPlayerS : NodeId -> Player -> Player -> Player-> (Player,Player,Player)                       
stepPlayerS nod curr pass dummy =
    let st = getState curr 
        newCurr = stepPlayer curr nod
    in case st of
        initState -> (newCurr, pass, dummy)
        pseudoMoveState -> (newCurr, pass, dummy)
        putState -> (newCurr, pass, {dummy | myFields = filter (\x -> x /= nod) dummy.myFields})
        slideState -> (newCurr, pass, {dummy | myFields = filter (\x -> x /= nod) dummy.myFields})
        jumpState -> (newCurr, pass, {dummy | myFields = filter (\x -> x /= nod) dummy.myFields})                
        pseudoTakeState -> (newCurr, pass, dummy)
        hasMillState -> ( newCurr 
                        , {pass | 
                            myFields = filter (\x -> x /= nod) pass.myFields,
                            numOfStones = pass.numOfStones -1 
                          }
                        , {dummy | myFields = nod::dummy.myFields}
                        )
        checkState -> let var = not << isEmpty <| (validTurns (newCurr,pass,dummy) nod)
                          pl = {curr | canMove = curr.var}
                      in ({pl | state =trans pl wizard, pl.playing = False}, {pass | pass.playing =True}, dummy)     

        
stepGame : Input -> Game -> Game
stepGame input g =
    let {x,y} = input
        curr = if g.pl1.playing = True then g.pl1 else g.pl2,
        -- something must check the mouseclicks 
        -- selectedNode = 
        -- targetNode = 
        (one, two, three) = stepPlayerS targetNode curr (opponent curr) g.plx,
        newPl1 = if one.name = "white" then one else two,
        newPl2 = if one.name = "white" then two else one,
        newPlx = three
        newStatus = if (one.state = endState || two.state = endState) then End else OnGoing
    in { g |
        pl1 = newPl1
        pl2 = newPl2
        plx = newPlx
        status = newStatus
    }

-- next transition for a player
--WARNING: this funtion is dependable of the order of the states in the stateMachine
trans : Player -> StateMachine-> State
trans pl m = case head << automatedStateSelect <| m pl of
            Just tuple -> snd tuple
            Nothing -> pl.state

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

