module EggCatcher.Types where


type Movement = {
  x :: Int,
  y :: Int,
  w :: Int,
  h :: Int
}

type GameState = {
   eggs :: Array Movement,
   bowlPos :: Movement,
   count :: Int,
   timer :: Int,
   gameState :: Boolean
}

data Direction = Left' | Right'