module View where

import Board exposing (..)
import Dict
import Graph exposing (..)
import Array
import List

(gameWidth, gameHeight) = (500,500)

type alias View = 
    {coords : List (Int,Int), pt : List NodeId, coordToNode : Dict.Dict (Int,Int) Int, nodeToCoord : Array.Array (Int,Int)}

realView : View
realView = let tmp= nodeList
           in { coords = ordCoord, pt = tmp, coordToNode = chainCoordToNode ordCoord tmp start, nodeToCoord = chainNodeToCoord ordCoord}

ordCoord : List (Int,Int)
ordCoord = [(-150,150),(0,150),(150,150),(-150,0),(150,0),(-150,-150),(0,-150),(150,-150),
            (-97,97),(0,97),(97,97),(-97,0),(97,0),(-97,-97),(0,-97),(97,-97),
            (-45,45),(0,45),(45,45),(-45,0),(45,0),(-45,-45),(0,-45),(45,-45) ]

start : Dict.Dict (Int,Int) NodeId
start = Dict.empty

chainCoordToNode : List (Int,Int) -> List NodeId -> Dict.Dict (Int,Int) NodeId -> Dict.Dict (Int,Int) NodeId
chainCoordToNode xs ys dic = List.foldr (\(a,b) c -> Dict.insert a b c) dic (zip xs ys)

chainNodeToCoord : List (Int,Int) -> Array.Array (Int,Int)
chainNodeToCoord = Array.fromList

zip : List a -> List b -> List (a,b)
zip xs ys = case xs of 
                [] -> []
                (v::vs) -> case ys of
                            [] -> []
                            (w::ws) -> (v,w)::(zip vs ws)
