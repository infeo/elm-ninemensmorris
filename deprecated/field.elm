module Board where
import Graph exposing (..)

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

type alias MillNode = Node {owner : String}
gamePoints : List Node Int
gamePoints = [ { id = 0, label = 000}      --the outer square
             , { id = 1, label = 001}
             , { id = 2, label = 002}
             , { id = 3, label = 010}
             , { id = 4, label = 012}
             , { id = 5, label = 020}
             , { id = 6, label = 021}
             , { id = 7, label = 022}
             , { id = 8, label = 100}     --the first inner square
             , { id = 9, label = 101}
             , { id = 10, label = 102}
             , { id = 11, label = 110}
             , { id = 12, label = 112}
             , { id = 13, label = 120}
             , { id = 14, label = 121}
             , { id = 15, label = 122}
             , { id = 16, label = 200}     --the second inner square
             , { id = 17, label = 201}
             , { id = 18, label = 202}
             , { id = 19, label = 210}
             , { id = 20, label = 212}
             , { id = 21, label = 220}
             , { id = 22, label = 221}
             , { id = 23, label = 222}
             ]

--Step 2: Create edges
gameLines : List Edge String
gameLines = [ { from=0, to=1, label ="000-001"}
            , { from=0, to=3, label ="000-010"}
            , { from=1, to=2, label ="001-002"}
            , { from=1, to=9, label ="001-101"}
            , { from=2, to=4, label ="002-012"}
            , { from=3, to=11, label ="010-110"}
            , { from=3, to=5, label ="010-020"}
            , { from=4, to=12, label ="012-112"}
            , { from=4, to=7, label ="012-022"}
            , { from=5, to=6, label ="020-021"}
            , { from=6, to=14, label ="021-121"}
            , { from=6, to=7, label ="021-022"}
            , { from=8, to=11, label ="100-110"}
            , { from=8, to=9, label ="100-101"}
            , { from=9, to=17, label ="100-201"}
            , { from=9, to=10, label ="100-102"}
            , { from=10, to=12, label ="102-112"}
            , { from=11, to=19, label ="110-210"}
            , { from=11, to=13, label ="110-120"}
            , { from=12, to=20, label ="112-212"}
            , { from=12, to=15, label ="112-122"}
            , { from=13, to=14, label ="120-121"}
            , { from=14, to=15, label ="121-122"}
            , { from=14, to=22, label ="121-221"}
            , { from=16, to=17, label ="200-201"}
            , { from=16, to=19, label ="200-210"}
            , { from=17, to=18, label ="201-202"}
            , { from=18, to=20, label ="202-212"}
            , { from=19, to=21, label ="210-220"}
            , { from=20, to=23, label ="212-222"}
            , { from=21, to=22, label ="220-221"}
            , { from=22, to=23, label ="221-222"}


--Step 3: creating the graph
gameBoard = symmetricClosure edgeMerger (fromNodeAndEdges gamePoints gameLines)
