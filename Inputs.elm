module Inputs (..) where

import Mouse
import Signal


type alias Input =
  ( Int, Int )


input : Signal Input
input =
  Signal.map transform (Signal.sampleOn Mouse.clicks Mouse.position)



{-

   --Signal wird immer erst auf zweiten Mausklick gesendet!
   input : Signal Input
   input = let sig1 = Signal.sampleOn Mouse.clicks Mouse.position
               funk : (Int,Int) -> ( Input ,Int ) -> ( Input,Int )
               funk = (\x ((a,b),c)-> ((transform x,a),c+1))
               std = (((0,0),(0,0)),0)
           in Signal.map fst (Signal.filter (isEven << snd) std (Signal.foldp funk std sig1))

   isEven : Int -> Bool
   isEven x = (x % 2) == 0
-}


transform : ( Int, Int ) -> ( Int, Int )
transform ( a, b ) =
  ( a - 300, b - 300 )
