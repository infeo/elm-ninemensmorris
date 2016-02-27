module Update (..) where

import Model exposing (..)
import Graph exposing (..)
import Dict
import List
import Maybe exposing (..)
import Debug exposing (crash)


--proofs, if a stone is in Mill or not


isInMill : Player -> NodeId -> Bool
isInMill pl nodeId =
  let
    ls =
      List.concatMap fromTupToList pl.mills
  in
    List.member nodeId ls


fromTupToList : ( a, a, a ) -> List a
fromTupToList ( one, two, three ) =
  [ one, two, three ]


checkMovement : Player -> Player -> BoardGraph -> List NodeId
checkMovement pl dummy gra=
  let
    freeNodes =
      dummy.myFields
  in
    case pl.numOfStones of
      3 ->
        freeNodes

      _ ->
        let
          ls =
            List.concatMap (\x -> fromIdToNeighbours x gra)  pl.myFields

          --alle erreichbaren Knoten, wenn der Spieler nur schieben kann
          ls2 =
            List.filter (\x -> not (List.member x pl.myFields)) ls

          --alle Knoten die nicht dem Spieler gehören
          ls3 =
            List.filter (\x -> List.member x freeNodes) ls2

          --alle Knoten die noch frei sind
        in
          ls3



-- returns True if the selected nodes are ok


validTurns : Game -> NodeId -> NodeId -> Bool
validTurns g fstNod sndNod =
  let
    ( st, curr, opp ) =
      allUNeed g

    dummy =
      g.plx

    freeNodes =
      dummy.myFields

    sndOk =
      case st of
        Slide ->
          let
            x =
              Graph.get fstNod (g.view).abstractRep 
          in
            case x of
              Just ctx ->
                List.any (\x -> x == sndNod) (List.filter (\x -> List.member x freeNodes) (alongOutgoingEdges ctx))

              Nothing ->
                False

        Jump ->
          List.member sndNod freeNodes

        _ ->
          True

    fstOk =
      case st of
        Put ->
          List.member fstNod freeNodes

        HasMill ->
          if List.length opp.myFields == List.length (List.concatMap fromTupToList opp.mills) then
            List.any (\x -> x == fstNod) opp.myFields
          else
            List.any (\x -> x == fstNod) opp.myFields && not (isInMill opp fstNod)

        Slide ->
          List.member fstNod curr.myFields

        Jump ->
          List.member fstNod curr.myFields

        _ ->
          True

    relOk =
      case st of
        Slide ->
          fstNod /= sndNod

        Jump ->
          fstNod /= sndNod

        HasMill ->
          fstNod == sndNod

        Put ->
          fstNod == sndNod

        _ ->
          True
  in
    relOk && fstOk && sndOk


fromIdToNeighbours : NodeId -> BoardGraph -> List NodeId
fromIdToNeighbours id gra=
  case (get id gra) of 
    Just ctx ->
      alongOutgoingEdges ctx

    Nothing ->
      []



{-
      Kombis für jede Komponente: (0,2) (0,1) (1,2)
      Je nach Bedarf muss vorne hinten oder in der Mitte eingefügt werden
      Dann überprüft man, ob die Elemente in der Liste sind.
      Dann braucht man wieder die KnotenIds
   --remeber: in the put State you can get two mills with  one stone

-}


getNewMills : Player -> NodeId -> BoardGraph -> List Mill
getNewMills pl nod graph=
  let
    max =
      \ns num val ->
        if num < 3 then
          ns ++ val
        else
          val

    tupLs =
      List.filter (\x -> List.member (fst x) pl.myFields) (List.map  fromContextToIdLabel  <| fst  (guidedBfs alongOutgoingEdges max [ nod ] [] graph)) 

    --Liste von (NodId, Labels)
    ( a, b, c ) =
      snd (fromIdToIdLabel graph nod)

    --Koordinaten des Punktes als Label
    select =
      \x ->
        case x of
          -- Welche weiteren Koordinaten müssen betrachtet werden?
          0 ->
            [ 1, 2 ]

          1 ->
            [ 0, 2 ]

          _ ->
            [ 0, 1 ]

    put =
      \pos ( u, v ) x ->
        case pos of
          -- Füge diese Koordinaten zu den beiden konstant bleibenden hinzu
          0 ->
            ( x, u, v )

          1 ->
            ( u, x, v )

          _ ->
            ( u, v, x )

    member' =
      \x ls ->
        case ls of
          -- Gibt es zu den nun generierten Koordinaten eine NodeId, die dem Spieler gehört
          [] ->
            Nothing

          ( id, label ) :: rest ->
            if x == label then
              Just id
            else
              member' x rest

    decide =
      \pos tup z ->
        let
          tupLs2 =
            List.filterMap (\x -> member' x tupLs) (List.map (put pos tup) (select z))
        in
          if (List.length tupLs2) == 2 then
            nod :: tupLs2
          else
            []

    toTuple =
      \ls ->
        case ls of
          x :: y :: z :: [] ->
            Just ( x, y, z )

          _ ->
            Nothing

    aMill =
      (decide 0 ( b, c ) a)

    bMill =
      (decide 1 ( a, c ) b)

    cMill =
      decide 2 ( a, b ) c
  in
    List.filterMap toTuple [ aMill, bMill, cMill ]


fromContextToIdLabel : NodeContext ( Int, Int, Int ) String -> ( NodeId, ( Int, Int, Int ) )
fromContextToIdLabel ctx =
  let
    nod =
      ctx.node
  in
    ( nod.id, nod.label )


fromIdToIdLabel : BoardGraph -> NodeId -> ( NodeId, ( Int, Int, Int ) )
fromIdToIdLabel graph x =
  case (get x graph) of
    Just ctx ->
      let
        nod =
          ctx.node
      in
        ( nod.id, nod.label )

    Nothing ->
      crash "IMPOSSIBLE: a nodeId is never deleted"


updateMills : Player -> NodeId -> NodeId -> BoardGraph -> ( Bool, List Mill )
updateMills pl oldId newId graph=
  let
    ls =
      List.filter (\( a, b, c ) -> a /= oldId && b /= oldId && c /= oldId) pl.mills

    ls2 =
      getNewMills pl newId graph
  in
    ( not (List.isEmpty ls2), List.concat (ls2 :: [ ls ]) )


stepPlayer : Game -> NodeId -> NodeId -> Player
stepPlayer g oldId newId =
  let
    (st, curr, opp) = allUNeed g
    graph = g.view.abstractRep
    newPl =
      case st of
        Put ->
          let
            tmp =
              { curr
                | numOfStonesInGame = curr.numOfStonesInGame + 1
                , myFields = newId :: curr.myFields
              }

            ( a, b ) =
              updateMills tmp oldId newId graph
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        Slide ->
          let
            tmp =
              { curr
                | myFields = newId :: (List.filter (\x -> x /= oldId) curr.myFields)
              }

            ( a, b ) =
              updateMills tmp oldId newId graph
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        Jump ->
          let
            tmp =
              { curr
                | myFields = newId :: (List.filter (\x -> x /= oldId) curr.myFields)
              }

            ( a, b ) =
              updateMills tmp oldId newId graph
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        HasMill ->
          { curr | closedMill = False }

        _ ->
          curr
  in
    trans newPl g.machine



{-
   Always the same tactic:
   1. Update the current Player
   2. Update the Passive Player
   3. Update the Dummy Player
-}


stepPlayerS : NodeId -> NodeId -> Game -> ( Player, Player, Player )
stepPlayerS oldId newId g =
  let
    ( st, curr, opp ) =
      allUNeed g

    graph = g.view.abstractRep
    
    dummy =
      g.plx

    newCurr =
      stepPlayer g oldId newId 

    tmp =
      opp.numOfStones
  in
    case st of
      Put ->
        ( newCurr, opp, { dummy | myFields = List.filter (\x -> x /= newId) dummy.myFields } )

      Slide ->
        ( newCurr, opp, { dummy | myFields = oldId :: (List.filter (\x -> x /= newId) dummy.myFields) } )

      Jump ->
        ( newCurr, opp, { dummy | myFields = oldId :: (List.filter (\x -> x /= newId) dummy.myFields) } )

      HasMill ->
        ( newCurr
        , { opp
            | myFields = List.filter (\x -> x /= newId) opp.myFields
            , numOfStones = tmp - 1
          }
        , { dummy | myFields = newId :: (dummy.myFields) }
        )

      Check ->
        let
          tmp_opp =
            { opp | canMove = not (List.isEmpty (checkMovement opp dummy graph)) }

          newOpp =
            trans tmp_opp (g.machine)
        in
          ( { curr
              | playing = False
              , canMove = not (List.isEmpty <| checkMovement curr dummy graph)
            }
          , { newOpp | playing = True }
          , dummy
          )

      _ ->
        ( newCurr, opp, dummy )


stepForward : ( NodeId, NodeId ) -> Game -> Game
stepForward ( fstNode, sndNode ) g =
  let
    ( one, two, newPlx ) =
      stepPlayerS fstNode sndNode g

    ( newPl1, newPl2 ) =
      case one.ty of
        White ->
          ( one, two )

        _ ->
          ( two, one )

    newStatus =
      if (one.state == endState || two.state == endState) then
        Win
      else if one.numOfStones == 3 && two.numOfStones == 3 then
        Draw
      else
        OnGoing
  in
    { g
      | pl1 = newPl1
      , pl2 = newPl2
      , plx = newPlx
      , status = newStatus
    }



-- do multiple state-transitions with one Click
-- @ inv: every time the Funktion is called, the state of the player changes


fastForward : ( NodeId, NodeId ) -> Game -> Game
fastForward inp g =
  let
    ( st, curr, opp ) =
      allUNeed g

    valid =
      validTurns g (fst inp) (snd inp)
  in
    if valid then
      case st of
        PseudoMove ->
          stepForward inp g

        PseudoTake ->
          let
            g1 =
              stepForward inp g

            curr =
              if g1.pl1.playing then
                g1.pl1
              else
                g1.pl2
          in
            case wrapper' curr of
              HasMill ->
                g1

              _ ->
                fastForward inp g1

        End ->
          g

        _ ->
          let
            g1 =
              stepForward inp g
          in
            fastForward inp g1
    else
      g


withinRange : ( Int, Int ) -> Int -> ( Int, Int ) -> Maybe ( Int, Int )
withinRange ( x, y ) delta ( a, b ) =
  if (x <= (a + delta) && x >= (a - delta)) && (y <= (b + delta) && y >= (b - delta)) then
    Just ( a, b )
  else
    Nothing


stepGame : ( Int, Int ) -> Game -> Game
stepGame input g =
  let
    ls =
      (g.view).coords

    mybNod =
      List.head (List.filterMap (withinRange input 10) ls) `andThen` (\x -> Dict.get x (g.view).coordToNode)

    ( st, curr, opp ) =
      allUNeed g

    fstNod =
      g.fstNod

    g' =
      { g | fstNod = Nothing }

    --reset in any case the fst-Click
  in
    case st of
      Jump ->
        case fstNod of
          Nothing ->
            { g' | fstNod = mybNod }

          -- is it the first Click? -> save it!
          Just u ->
            case mybNod of
              Nothing ->
                g'

              Just v ->
                fastForward ( u, v ) g'

      --otherwise only proceed if both clicks are valid
      Slide ->
        case fstNod of
          Nothing ->
            { g' | fstNod = mybNod }

          -- same as Jump
          Just u ->
            case mybNod of
              Nothing ->
                g'

              Just v ->
                fastForward ( u, v ) g'

      HasMill ->
        case mybNod of
          Nothing ->
            g'

          Just u ->
            fastForward ( u, u ) g'

      Put ->
        case mybNod of
          Nothing ->
            g'

          Just u ->
            fastForward ( u, u ) g'

      _ ->
        fastForward ( 0, 0 ) g'



--ugly


allUNeed : Game -> ( FakeState, Player, Player )
allUNeed g =
  let
    ( curr, opp ) =
      if g.pl1.playing then
        ( g.pl1, g.pl2 )
      else
        ( g.pl2, g.pl1 )
  in
    ( wrapper' curr, curr, opp )
