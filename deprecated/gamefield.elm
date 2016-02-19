module Gamefield where
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

--type Player =  white Name myStones MoveState MillState | black Name myStones MoveState MillState
--type alias Name = String
--type alias myStones = List Stones
--type alias Stones = (Int, Int)
--type MoveState = Put | Slide | Jump
--type MillState = Jop | Nop
--type alias Point = (Int,Int)





field : Form
field = group 
        [ outlined defaultLine (square 100.0)
        , outlined defaultLine (square 65.0)
        , outlined defaultLine (square 30.0) 
        , traced defaultLine (segment (-50,0) (50,0))
        , traced defaultLine (segment (0,-50) (0,50))]

main : Element
main = collage 1000 1000 [scale 3.0 field]


{-
dynamic_field : State ->

my_main : Signal Element
my_main = Signal.map dynamic_field current_state
-}
