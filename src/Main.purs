module Main where

import Prelude
import PrestoDOM.Types
import PrestoDOM.Elements
import PrestoDOM.Events
import EggCatcher.Values
import EggCatcher.Types
import PrestoDOM.Properties
import Data.Maybe
import Data.Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Plus ((<|>))
import PrestoDOM.Core
import PrestoDOM.Util as U
import FRP.Event.Keyboard (down, up)
import FRP(FRP)
import FRP.Event (Event)
import FRP.Behavior (step)
import FRP.Event.Time (animationFrame, withTime, interval)
import FRP.Behavior (sample_)
import FRP.Behavior.Keyboard (keys, key)
import Data.Set(member)
import Data.Array((..))
import Data.Traversable


-- foreign import click :: MEvent
-- foreign import getId :: Int

widget :: forall i a. GameState -> VDom (Array (Prop i)) a
widget state = relativeLayout
              [ height Match_Parent
              , width Match_Parent
              , background "#000000"
              , orientation "vertical"
              ]
              [
                linearLayout
                  [ height $ V boxHeight
                  , width $ V 500
                  , background "#FFFFFF"
                  , orientation "vertical"
                  , margin ((show groundX) <> ",0,0,0")
                  ]
                  []
                ,

                relativeLayout
                  [ height Match_Parent
                  , width Match_Parent
                  ]
                  (map(\image -> (getImageObject image)) state.eggs),

                  imageView
                  [ width ( V $ state.bowlPos.w)
                  , height (V $ state.bowlPos.h)
                  , margin (show (groundX + state.bowlPos.x) <> "," <> show (boxHeight - state.bowlPos.y) <> ",0,0")
                  , imageUrl "bowl"
                  ]
                ,

                linearLayout
                [ height $ V 75
                , width $ V 200
                , gravity "center"
                , margin "300,450,0,0"
                , typeface "bold" 

                ]
                  [
                    textView
                    [width $ V 200
                    , height $ V 20
                    , textSize "24"
                    , text "EGG CATCHER"
                    , margin "200,0,0,0"
                    , color "#FFFFFF"
                    ]
                  ]
                ,

                linearLayout
                [ height $ V 75
                , width $ V 150
                , margin "75,100,0,0"
                , orientation "vertical"
                ]
                  [

                    textView
                    [width Match_Parent
                    ,height $ V 20
                    ,textSize "40"
                    , text "TIMER"
                    , typeface "bold" 
                    , color "#FFFFFF"
                    ]
                    ,
                    textView
                    [width Match_Parent
                    ,height $ V 20
                    ,textSize "40"
                    , text $ show (state.timer) 
                    , margin "30,50,0,0"
                    , color "#FFFFFF"
                    ]
                  ]
                  ,

                linearLayout
                [ height $ V 75
                , width $ V 150
                , margin ( show (groundX + 500 + 40) <> ",100,0,0")
                , orientation "vertical"
                ]
                  [

                    textView
                    [width Match_Parent
                    ,height $ V 20
                    ,textSize "40"
                    , text "SCORE"
                    , typeface "bold" 
                    , color "#FFFFFF"
                    ]
                    ,
                    textView
                    [width Match_Parent
                    ,height $ V 20
                    ,textSize "40"
                    , text $ show (state.count) 
                    , margin "30,50,0,0"
                    , color "#FFFFFF"
                    ]
                  ]

                  ,

                  (getGameState state)

              ]


getGameState :: forall i a. GameState -> VDom (Array (Prop i)) a
getGameState state = relativeLayout
                      [ width $ V 500
                      , height $ V 200
                      , margin ( show (groundX + 140) <> ",80,0,0")
                      , visibility ( if state.gameState 
                                        then "gone"
                                        else "visible")
                      ]
                      [
                      textView
                      [ width $ V 400
                      , height $ V 80
                      , fontStyle "Courier Header"
                      , text "Game Over"
                      , textSize "45"
                      , typeface "bold" 
                      , margin "0,80,0,0"
                      , color "#F51D1D"
                      ]
                      ]   

eggVal ::forall a. Int -> Eff(random :: RANDOM | a) Movement
eggVal a = 
   randomInt 0 100 >>= \n ->
    randomInt 0 20 >>= \m ->
        pure { x : ((n * 5) + a ) + 280, y : (a * -200), w : 24, h : 24} 

-- 
collitionCase :: forall a. Movement -> Movement -> Eff _ Boolean
collitionCase bowl egg = do 
  -- log $ show (egg.x - groundX) <> " : " <> show egg.w <> " : " <> show bowl.x <> " : " <> show bowl.w
  log $ show (egg.y) <> " : " <> show (bowl.y)
  pure (((egg.x + egg.w) - groundX) > ( bowl.x +20)&& 
    ((egg.x) - groundX) <= ((bowl.x + bowl.w) - 20) &&
    egg.y > ((boxHeight - bowl.y)))

generateEggs = sequence (map (\i -> eggVal i) (1..7))


getImageObject :: forall i a. Movement -> VDom (Array (Prop i)) a
getImageObject imageMetrics = imageView
                                [ width $ V imageMetrics.w
                                , height $ V imageMetrics.h
                                , margin ((show imageMetrics.x) <> ","<>(show imageMetrics.y)<>",0,0")
                                , imageUrl "eggBrown"
                                ]

main = do
  --- Init State {} empty record--
  U.initializeState
  
  --- Update State ----
  eggs <- generateEggs
  _ <- U.updateState "eggs" eggs
  _ <- U.updateState "count" 0
  _ <- U.updateState "timer" 60
  _ <- U.updateState "gameState" isGameOver
  state <- U.updateState "bowlPos" {x : 210, y : 40,w : 50, h: 50}

  ---- Render Widget ---
  U.render (widget state) listen

  pure unit


updateEggPosition (state :: GameState) = U.updateState "eggs" (map (\o-> o {y= ((o.y+(if o.y > 380
                                                                                        then -500
                                                                                        else 0
                                                                                      )) + 3)}) state.eggs)

moveBowl direction = do
  state <- U.getState
  if (state.gameState == isGameOver)
    then do
      case direction of
         Nothing -> U.getState
         Just Left' -> do
          if(state.bowlPos.x <= 0)
            then U.updateState "bowlPos" (state.bowlPos{x = 0})
            else U.updateState "bowlPos" (state.bowlPos{x = state.bowlPos.x - 7})
              
         Just Right' -> do
          if (state.bowlPos.x >= (500 - state.bowlPos.w))
              then U.updateState "bowlPos" (state.bowlPos{x = (500 - state.bowlPos.w)})
              else U.updateState "bowlPos" (state.bowlPos{x = state.bowlPos.x + 7})
    else U.getState

scoreup a = if a
              then do
               state <- U.getState
               U.updateState "count" (state.count + 1)
              else
                U.getState

checkColl a b = if a
                  then b {y = -24}
                  else b

updateTimer _ = do
  state <- U.getState
  if(state.timer == 0)
    then U.updateState "gameState" $ not isGameOver
    else U.updateState "timer" (state.timer - 1)

moveEgg :: forall t257 t270.t257-> Eff( console :: CONSOLE| t270){ eggs :: Array{ x :: Int, y :: Int, w :: Int, h :: Int}, bowlPos :: { x :: Int, y :: Int, w :: Int, h :: Int}, count :: Int, timer :: Int, gameState :: Boolean}
moveEgg _ = do
  (state :: GameState) <- U.getState
  if (state.gameState == isGameOver)
    then do 
      c <- (traverse (collitionCase state.bowlPos) state.eggs)
      _ <- (traverse scoreup c)
      _ <- U.updateState "eggs" (zipWith checkColl c state.eggs) 
      _ <- updateEggPosition state
      pure state
    else U.getState



shouldMove :: Event (Maybe Direction)
shouldMove = eval <$> withTime down <*> withTime up
             where eval dWT uWT = if dWT.time > uWT.time
                                  then
                                    if dWT.value == 37
                                      then Just Left'
                                      else Just Right'
                                  else Nothing
  

listen :: forall a.Eff( frp :: FRP, console :: CONSOLE| a)(Eff( frp :: FRP, console :: CONSOLE| a ) Unit)
listen = do

  state <- U.getState

  let b = moveBowl <$> step Nothing shouldMove
  let events = animationFrame

  let animBehav = moveEgg <$> (step unit animationFrame)
  let animEve = animationFrame

  let timerBehav = updateTimer <$> (key 32)
  let timerEve = interval 1000

  _ <- U.patch widget animBehav animEve
  _ <- U.patch widget timerBehav timerEve

  U.patch widget b events