module Board (board, nodeList, realView, View) where

import Graph exposing (..)
import Dict
import Array
import List
import Graphics.Collage exposing (..)
import Color

--represent the mill-gameBoard as a Graph
{-
   -Step 1: create your Nodes
   -Little Codedictionary:
   -Length: 3 --> XXX
   -First Parameter: Describes, which Square is meant, from outside in
   -There are Three Squares: 0,1,2
   -Second Parameter: Describes, which Point in y-direction is meant
   -possibilities: 0 (Top), 1(Middle), 3 (Bottom)
   -Third Parameter: same as Second only in x-direction
   - There are 3 possibilities: 0 (Left), 1(Middle), 2(right)
-}


gamePoints : List (Node ( Int, Int, Int ))
gamePoints =
  [ { id = 0, label = ( 0, 0, 0 ) }
    --the outer square
  , { id = 1, label = ( 0, 0, 1 ) }
  , { id = 2, label = ( 0, 0, 2 ) }
  , { id = 3, label = ( 0, 1, 0 ) }
  , { id = 4, label = ( 0, 1, 2 ) }
  , { id = 5, label = ( 0, 2, 0 ) }
  , { id = 6, label = ( 0, 2, 1 ) }
  , { id = 7, label = ( 0, 2, 2 ) }
  , { id = 8, label = ( 1, 0, 0 ) }
    --the first inner square
  , { id = 9, label = ( 1, 0, 1 ) }
  , { id = 10, label = ( 1, 0, 2 ) }
  , { id = 11, label = ( 1, 1, 0 ) }
  , { id = 12, label = ( 1, 1, 2 ) }
  , { id = 13, label = ( 1, 2, 0 ) }
  , { id = 14, label = ( 1, 2, 1 ) }
  , { id = 15, label = ( 1, 2, 2 ) }
  , { id = 16, label = ( 2, 0, 0 ) }
    --the second inner square
  , { id = 17, label = ( 2, 0, 1 ) }
  , { id = 18, label = ( 2, 0, 2 ) }
  , { id = 19, label = ( 2, 1, 0 ) }
  , { id = 20, label = ( 2, 1, 2 ) }
  , { id = 21, label = ( 2, 2, 0 ) }
  , { id = 22, label = ( 2, 2, 1 ) }
  , { id = 23, label = ( 2, 2, 2 ) }
  ]



--Step 2: Create edges


gameLines : List (Edge String)
gameLines =
  [ { from = 0, to = 1, label = "000-001" }
  , { from = 0, to = 3, label = "000-010" }
  , { from = 1, to = 2, label = "001-002" }
  , { from = 1, to = 9, label = "001-101" }
  , { from = 2, to = 4, label = "002-012" }
  , { from = 3, to = 11, label = "010-110" }
  , { from = 3, to = 5, label = "010-020" }
  , { from = 4, to = 12, label = "012-112" }
  , { from = 4, to = 7, label = "012-022" }
  , { from = 5, to = 6, label = "020-021" }
  , { from = 6, to = 14, label = "021-121" }
  , { from = 6, to = 7, label = "021-022" }
  , { from = 8, to = 11, label = "100-110" }
  , { from = 8, to = 9, label = "100-101" }
  , { from = 9, to = 17, label = "100-201" }
  , { from = 9, to = 10, label = "100-102" }
  , { from = 10, to = 12, label = "102-112" }
  , { from = 11, to = 19, label = "110-210" }
  , { from = 11, to = 13, label = "110-120" }
  , { from = 12, to = 20, label = "112-212" }
  , { from = 12, to = 15, label = "112-122" }
  , { from = 13, to = 14, label = "120-121" }
  , { from = 14, to = 15, label = "121-122" }
  , { from = 14, to = 22, label = "121-221" }
  , { from = 16, to = 17, label = "200-201" }
  , { from = 16, to = 19, label = "200-210" }
  , { from = 17, to = 18, label = "201-202" }
  , { from = 18, to = 20, label = "202-212" }
  , { from = 19, to = 21, label = "210-220" }
  , { from = 20, to = 23, label = "212-222" }
  , { from = 21, to = 22, label = "220-221" }
  , { from = 22, to = 23, label = "221-222" }
  ]



--Step 3: creating the graph


tmp : Graph ( Int, Int, Int ) String
tmp =
  fromNodesAndEdges gamePoints gameLines


nodeList : List NodeId
nodeList =
  nodeIds tmp


board : Graph ( Int, Int, Int ) String
board =
  symmetricClosure merger (tmp)



--merger : NodeId -> NodeId -> e -> e -> e


merger from to outgoingLabel incomingLabel =
  outgoingLabel



-- the graphical representation


type alias View =
  { abstractRep : Graph (Int,Int,Int) String, concreteRep : Form, coords : List ( Int, Int ), pt : List NodeId, coordToNode : Dict.Dict ( Int, Int ) Int, nodeToCoord : Array.Array ( Int, Int ) }


realView : View
realView =
    { abstractRep = symmetricClosure merger (tmp), concreteRep = realField, coords = ordCoord, pt = nodeList, coordToNode = chainCoordToNode ordCoord nodeList start, nodeToCoord = chainNodeToCoord ordCoord }


ordCoord : List ( Int, Int )
ordCoord =
  [ ( -150, 150 )
  , ( 0, 150 )
  , ( 150, 150 )
  , ( -150, 0 )
  , ( 150, 0 )
  , ( -150, -150 )
  , ( 0, -150 )
  , ( 150, -150 )
  , ( -97, 97 )
  , ( 0, 97 )
  , ( 97, 97 )
  , ( -97, 0 )
  , ( 97, 0 )
  , ( -97, -97 )
  , ( 0, -97 )
  , ( 97, -97 )
  , ( -45, 45 )
  , ( 0, 45 )
  , ( 45, 45 )
  , ( -45, 0 )
  , ( 45, 0 )
  , ( -45, -45 )
  , ( 0, -45 )
  , ( 45, -45 )
  ]


start : Dict.Dict ( Int, Int ) NodeId
start =
  Dict.empty


chainCoordToNode : List ( Int, Int ) -> List NodeId -> Dict.Dict ( Int, Int ) NodeId -> Dict.Dict ( Int, Int ) NodeId
chainCoordToNode xs ys dic =
  Dict.fromList (List.map2 (,) xs ys)


chainNodeToCoord : List ( Int, Int ) -> Array.Array ( Int, Int )
chainNodeToCoord =
  Array.fromList

realField : Form
realField =
  group
    [ filled Color.grey (square 400)
    , outlined defaultLine (square 300.0)
    , outlined defaultLine (square 195.0)
    , outlined defaultLine (square 90.0)
    , traced defaultLine (segment ( -150, 0 ) ( -45, 0 ))
    , traced defaultLine (segment ( 45, 0 ) ( 150, 0 ))
    , traced defaultLine (segment ( 0, 45 ) ( 0, 150 ))
    , traced defaultLine (segment ( 0, -150 ) ( 0, -45 ))
    ]
