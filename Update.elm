module Update where

import Model exposing (..)
import Inputs exposing (..)
import Graph exposing (..)
import Dict
import Signal
import Array 
import List
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color
import Maybe exposing (..)

import Mouse

--for easy use
wizard = initGame.machine -- : StateMachine Player
magic = initGame.board
dragon = (initGame.plx).myFields

--proofs, if a stone is in Mill or not
isInMill : Player -> NodeId -> Bool
isInMill pl nodeId = let ls = List.concatMap helper3 pl.mills
                        in List.member nodeId ls

helper3 : (a,a,a) -> List a
helper3 (one,two,three) = [one,two,three]

checkMovement : Player -> Player -> List NodeId
checkMovement pl dummy = let freeNodes = dummy.myFields
                         in case pl.numOfStones of
                                3 -> freeNodes
                                _ -> let ls = List.concatMap helper2 pl.myFields --alle erreichbaren Knoten, wenn der Spieler nur schieben kann
                                         ls2= List.filter (\x-> not (List.member x pl.myFields)) ls --alle Knoten die nicht dem Spieler gehören
                                         ls3 = List.filter (\x -> List.member x freeNodes) ls2 --alle Knoten die noch frei sind
                                     in ls3

-- returns True if the selected nodes are ok
validTurns : Player -> Player -> Player -> NodeId -> NodeId -> Bool
validTurns curr pass dummy fstNod sndNod = 
    let st = wrapper' curr
        freeNodes = dummy.myFields
        sndOk = case st of
                    Slide -> let x = Graph.get fstNod magic
                              in case x of
                                Just ctx -> List.any (\x -> x == sndNod) (List.filter (\x-> List.member x freeNodes) (alongOutgoingEdges ctx)) 
                                Nothing ->  False --you'll never get here
                    HasMill -> List.any (\x -> x == sndNod) pass.myFields && not (isInMill pass sndNod)
                    Jump -> List.member sndNod freeNodes
                    Put -> List.member sndNod freeNodes
                    _ -> True
        fstOk = case st of
                    Put -> List.member fstNod freeNodes
                    HasMill -> List.any (\x -> x == fstNod) pass.myFields && not (isInMill pass fstNod)
                    Slide -> List.member fstNod curr.myFields
                    Jump -> List.member fstNod curr.myFields
                    _ -> True
        relOk = case st of
                Slide -> fstNod /= sndNod
                Jump -> fstNod /= sndNod
                HasMill -> fstNod == sndNod
                Put -> fstNod == sndNod
                _ -> True
    in relOk && fstOk && sndOk 




helper2 : NodeId -> List NodeId
helper2 id = case (get id magic) of
                Just(ctx) -> alongOutgoingEdges ctx 
                Nothing -> []

{-
    Only works with magic
  Kombis für jede Komponente: (0,2) (0,1) (1,2)
  Je nach Bedarf muss vorne hinten oder in der Mitte eingefügt werden
  Dann überprüft man, ob die Elemente in der Liste sind.
  Dann braucht man wieder die KnotenIds
-}                       
getNewMills : Player -> NodeId -> List Mill  --remeber: in the put State you can get two mills with  one stone
getNewMills pl nod = let  max = \ ns num val -> if num <3 then ns ++ val else val
                          tupLs = List.filter (\x -> List.member (fst x) pl.myFields) ( List.map helper' (fst (guidedBfs alongOutgoingEdges max [nod] [] magic))) --Liste von (NodId, Labels)
                          (a,b,c) = snd (helper nod) --Koordinaten des Punktes als Label
                          select = \x-> case x of -- Welche weiteren Koordinaten müssen betrachtet werden?
                                        0 -> [1,2]
                                        1 -> [0,2]                       
                                        _ -> [0,1]
                          put = \pos (u,v) x -> case pos of -- Füge diese Koordinaten zu den beiden konstant bleibenden hinzu
                                                0 -> (x,u,v)
                                                1 -> (u,x,v)
                                                _ -> (u,v,x)
                          member' = \x ls -> case ls of      -- Gibt es zu den nun generierten Koordinaten eine NodeId, die dem Spieler gehört
                                                [] -> Nothing
                                                ((id,label)::rest) -> if x==label then Just id else  member' x rest
                          decide = \pos tup z ->  let tupLs2 = List.filterMap (\x -> member' x tupLs) (List.map (put pos tup) (select z))
                                                    in if (List.length tupLs2) == 2 then nod::tupLs2 else []
                          toTuple = \ls -> case ls of
                                            x::y::z::[] -> Just(x,y,z)
                                            _ -> Nothing -- again a point, where the programm will never come to
                          aMill = (decide 0 (b,c) a) 
                          bMill = (decide 1 (a,c) b)
                          cMill = decide 2 (a,b) c
                        in List.filterMap toTuple [aMill,bMill,cMill]


helper' : NodeContext (Int,Int,Int) String -> (NodeId,(Int,Int,Int)) --not to confuse with mills!
helper' ctx = let nod = ctx.node 
                in (nod.id,nod.label)
                                                                             
helper : NodeId -> (NodeId,(Int,Int,Int)) --not to confuse with mills!
helper x = case (get x magic) of
            Just (ctx) -> let nod = ctx.node in (nod.id,nod.label)
            Nothing -> (0,(0,0,0)) -- VERY UGLY, but impossible to get here,  i'm supposed to write something

updateMills : Player -> NodeId -> NodeId -> (Bool,List Mill)
updateMills pl oldId newId= let ls = List.filter (\(a,b,c) -> a /= oldId && b /= oldId && c /= oldId) pl.mills
                                ls2 = getNewMills pl newId
                                in (not (List.isEmpty ls2), List.concat (ls2 :: [ls]))

stepPlayer : Player -> NodeId -> NodeId -> Player
stepPlayer pl oldId newId = 
    let st = wrapper pl
        newPl = if st == putState
                then let tmp = { pl | 
                                    numOfStonesInGame = pl.numOfStonesInGame + 1,
                                    myFields = newId::pl.myFields 
                               }
                         (a,b) = updateMills tmp oldId newId
                     in { tmp | 
                          mills = b,
                          closedMill = a
                        }
                else
                if st == slideState
                then let tmp = { pl |
                                    myFields = newId::(List.filter (\x -> x/= oldId) pl.myFields)
                               } 
                         (a,b) = updateMills tmp oldId newId
                     in { tmp |
                            mills = b,
                            closedMill = a
                        }
                 else 
                 if st == jumpState 
                 then let tmp = { pl |
                                    myFields = newId::(List.filter (\x -> x/= oldId) pl.myFields)
                                }
                          (a,b) = updateMills tmp oldId newId
                      in { tmp |
                            mills = b,
                            closedMill = a
                         }
                 else
                 if st == hasMillState
                 then {pl | closedMill = False}
                 else pl
    in trans newPl wizard 


 
{-
Always the same tactic:
1. Update the current Player
2. Update the Passive Player
3. Update the Dummy Player
-} 
stepPlayerS : NodeId -> NodeId -> Player -> Player -> Player-> (Player,Player,Player)                       
stepPlayerS oldId newId curr pass dummy =
    let st = wrapper' curr
        newCurr = stepPlayer curr oldId newId
        tmp = pass.numOfStones
    in case st of
        Put -> (newCurr, pass, {dummy | myFields = List.filter (\x -> x /= newId) dummy.myFields })
        Slide -> (newCurr, pass, {dummy | myFields = oldId :: (List.filter (\x -> x /= newId) dummy.myFields)})
        Jump -> (newCurr, pass, {dummy | myFields = oldId:: (List.filter (\x -> x /= newId) dummy.myFields)})
        HasMill -> ( newCurr 
                    , {pass | 
                        myFields = List.filter (\x -> x /= newId) pass.myFields,
                        numOfStones = tmp - 1 
                      }
                    , {dummy | myFields = newId::(dummy.myFields)}
                   )
        Check -> let tmp_pass = { pass | canMove = not (List.isEmpty <| checkMovement pass dummy) } 
                     newPass = trans tmp_pass wizard
                 in ( {curr | 
                        playing = False,
                        canMove = not (List.isEmpty <| checkMovement curr dummy)
                      }
                    , {newPass | playing = True}
                    , dummy
                    )
        _ -> (newCurr, pass, dummy)

  
stepForward : (NodeId,NodeId) -> Game -> Game
stepForward (fstNode,sndNode) g =
    let (curr,opp) = if g.pl1.playing then (g.pl1,g.pl2) else (g.pl2,g.pl1)
        (one, two, three) = stepPlayerS fstNode sndNode curr opp g.plx
        (newPl1, newPl2) = if one.name == "White Hat" then (one,two) else (two,one) --UGLY
        newPlx = three
        newStatus = if (one.state == endState || two.state == endState) then End else OnGoing
    in { g |
        pl1 = newPl1,
        pl2 = newPl2,
        plx = newPlx,
        status = newStatus
        }
        
-- setzt voraus, dass sich bei jedem stepForward der Zustand eines players sich ändert
-- do multiple state-transitions with one Click
fastForward: (NodeId,NodeId) -> Game -> Game
fastForward inp g = let (curr,opp) = if g.pl1.playing then (g.pl1,g.pl2) else (g.pl2,g.pl1)
                        valid = validTurns curr opp g.plx (fst inp) (snd inp)
                        st = wrapper' curr
                    in if valid 
                        then case st of
                            PseudoMove -> stepForward inp g
                            PseudoTake -> let g1 = stepForward inp g
                                              curr = if g1.pl1.playing then g1.pl1 else g1.pl2
                                          in case wrapper' curr of
                                            HasMill -> g1
                                            _ -> fastForward inp g1
                            EndSt -> g
                            _ -> let g1 = stepForward inp g
                                 in fastForward inp g1
                        else g  
                        
-- with this implementation the function validTurns can be refactored        
stepGame : Input -> Game -> Game
stepGame input g =
    let ls = (g.view).coords
        mybNod = List.head (List.filterMap (withinRange input 10) ls) `andThen` (\x -> Dict.get x (g.view).coordToNode)
        (curr,opp) = if g.pl1.playing then (g.pl1,g.pl2) else (g.pl2,g.pl1)
        st = wrapper' curr
        fstNod = g.fstNod
        g' = {g | fstNod = Nothing} --reset in any case the fst-Click
    in case st of
        Jump -> case fstNod of
                Nothing -> {g' | fstNod = mybNod} -- is it the first Click? -> save it!
                Just u -> case mybNod of
                            Nothing -> g'
                            Just v -> fastForward (u,v) g' --otherwise only proceed if both clicks are valid
        Slide -> case fstNod of
                Nothing -> {g' | fstNod = mybNod} -- same as Jump
                Just u -> case mybNod of
                            Nothing -> g'
                            Just v -> fastForward (u,v) g'
        HasMill -> case mybNod of
                    Nothing -> g'
                    Just u -> fastForward (u,u) g'
        Put -> case mybNod of
                Nothing -> g'
                Just u -> fastForward (u,u) g'
        _ -> fastForward (0,0) g' -- UGLY

------------------
gameState : Signal.Signal Game
gameState = Signal.foldp stepGame initGame input

display :Game -> (Int,Int) -> Element
display g mouse = case g.status of
                    OnGoing -> displayOngoing g mouse
                    End -> displayEnd g

displayOngoing : Game -> (Int,Int)-> Element
displayOngoing g test =
    let pl1 = g.pl1
        pl2 = g.pl2
        view = g.view
        curr = if pl1.playing then pl1 else pl2
        whiteStones = List.filterMap (\x -> Array.get x view.nodeToCoord) pl1.myFields
        blackStones = List.filterMap (\x -> Array.get x view.nodeToCoord) pl2.myFields
        showStones = \color ls ->  group <| List.map (\(x,y) -> move ( toFloat x, toFloat (-y)) (filled color (circle 10))) ls
    in container 600 600 middle <|
        collage 500 500
            [ realField
            , showPlayer pl1 |> move (-100, 175)
            , showPlayer pl2 |> move (100, 175)
            , toForm (show g.status)
            , toForm (show test) |> move (0, -175)
            , showStones Color.white whiteStones
            , showStones Color.black blackStones
            ]

displayEnd : Game -> Element
displayEnd g = let winner = if g.pl1.state == endState then "Player 2 wins" else "Player 1 wins"
               in container 600 600 middle <|
                collage 500 500
                    [ toForm (show winner) ]

main : Signal.Signal Element       
main = Signal.map2 display gameState (Mouse.position)


realField : Form
realField = group 
            [ filled Color.grey (square 400)
            , outlined defaultLine (square 300.0) -- 50 * 3 = 150
            , outlined defaultLine (square 195.0) -- 32.5 *3 = 90+6+1.5 = 97.5
            , outlined defaultLine (square 90.0)  -- 15 * 3 = 30+15= 45
            , traced defaultLine (segment (-150,0) (-45,0))
            , traced defaultLine (segment (45,0) (150,0))
            , traced defaultLine (segment (0,45) (0,150))
            , traced defaultLine (segment (0,-150) (0,-45))]

withinRange : (Int,Int) -> Int -> (Int,Int) -> Maybe (Int,Int)
withinRange (x,y) delta (a,b) = if (x <= (a+delta) && x >= (a-delta)) && (y <= (b+delta) && y >= (b-delta)) 
                                then Just (a,b)
                                else Nothing
                                
showPlayer : Player -> Form
showPlayer pl = let ma = toForm << show
                in group
                    [ ma pl.name
                    , ma pl.playing |>  move (0,10)
                    , ma pl.numOfStones |>  move (0,20)
                    , ma pl.numOfStonesInGame |>  move (0,30)
                    , ma pl.closedMill |>  move (0,40)
                    , ma pl.canMove |>  move (0,50)
                    , ma pl.state |>  move (0,60)
                    , ma pl.mills |> move (0,70)
                    ]

