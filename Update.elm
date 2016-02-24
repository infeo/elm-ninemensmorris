module Update (..) where

import Model exposing (..)
import Graph exposing (..)
import Dict
import List
import Maybe exposing (..)
import Debug exposing (crash)


--for easy use


wizard =
  initGame.machine


magic =
  (initGame.view).abstractRep


dragon =
  (initGame.plx).myFields



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


checkMovement : Player -> Player -> List NodeId
checkMovement pl dummy =
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
            List.concatMap fromIdToNeighbours pl.myFields

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
              Graph.get fstNod magic
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


fromIdToNeighbours : NodeId -> List NodeId
fromIdToNeighbours id =
  case (get id magic) of
    Just ctx ->
      alongOutgoingEdges ctx

    Nothing ->
      []



{-
        Only works with magic
      Kombis für jede Komponente: (0,2) (0,1) (1,2)
      Je nach Bedarf muss vorne hinten oder in der Mitte eingefügt werden
      Dann überprüft man, ob die Elemente in der Liste sind.
      Dann braucht man wieder die KnotenIds
   --remeber: in the put State you can get two mills with  one stone

-}


getNewMills : Player -> NodeId -> List Mill
getNewMills pl nod =
  let
    max =
      \ns num val ->
        if num < 3 then
          ns ++ val
        else
          val

    tupLs =
      List.filter (\x -> List.member (fst x) pl.myFields) (List.map fromContextToIdLabel (fst (guidedBfs alongOutgoingEdges max [ nod ] [] magic)))

    --Liste von (NodId, Labels)
    ( a, b, c ) =
      snd (fromIdToIdLabel nod)

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


fromIdToIdLabel : NodeId -> ( NodeId, ( Int, Int, Int ) )
fromIdToIdLabel x =
  case (get x magic) of
    Just ctx ->
      let
        nod =
          ctx.node
      in
        ( nod.id, nod.label )

    Nothing ->
      crash "IMPOSSIBLE: a nodeId is never deleted"


updateMills : Player -> NodeId -> NodeId -> ( Bool, List Mill )
updateMills pl oldId newId =
  let
    ls =
      List.filter (\( a, b, c ) -> a /= oldId && b /= oldId && c /= oldId) pl.mills

    ls2 =
      getNewMills pl newId
  in
    ( not (List.isEmpty ls2), List.concat (ls2 :: [ ls ]) )


stepPlayer : Player -> NodeId -> NodeId -> Player
stepPlayer pl oldId newId =
  let
    st =
      wrapper' pl

    newPl =
      case st of
        Put ->
          let
            tmp =
              { pl
                | numOfStonesInGame = pl.numOfStonesInGame + 1
                , myFields = newId :: pl.myFields
              }

            ( a, b ) =
              updateMills tmp oldId newId
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        Slide ->
          let
            tmp =
              { pl
                | myFields = newId :: (List.filter (\x -> x /= oldId) pl.myFields)
              }

            ( a, b ) =
              updateMills tmp oldId newId
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        Jump ->
          let
            tmp =
              { pl
                | myFields = newId :: (List.filter (\x -> x /= oldId) pl.myFields)
              }

            ( a, b ) =
              updateMills tmp oldId newId
          in
            { tmp
              | mills = b
              , closedMill = a
            }

        HasMill ->
          { pl | closedMill = False }

        _ ->
          pl
  in
    trans newPl wizard



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

    dummy =
      g.plx

    newCurr =
      stepPlayer curr oldId newId

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
            { opp | canMove = not (List.isEmpty <| checkMovement opp dummy) }

          newOpp =
            trans tmp_opp wizard
        in
          ( { curr
              | playing = False
              , canMove = not (List.isEmpty <| checkMovement curr dummy)
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

        EndSt ->
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


stepGame : (Int,Int) -> Game -> Game
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
