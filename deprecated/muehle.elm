module Muehle where
import State

{-
idee: benutzte foldp!
foldp : (a -> state -> state) -> init -> MouseClick -> Signal state

-}


changeState : validAction -> Player -> Player
changeState a pl = trans pl



main = map whatYouSee (foldp changeState white MouseClick)
