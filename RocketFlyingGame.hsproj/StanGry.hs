module StanGry where

import Graphics.SpriteKit

  
data StanObiektu = StanPrzeszkod [Int]      -- unbound sequence of random numbers
               | NoState

type LambdaNode = Node StanObiektu

randomInt :: StanObiektu -> (StanObiektu, Int)
randomInt NoState             = (NoState, 0)
randomInt (StanPrzeszkod (i:is)) = (StanPrzeszkod is, i)


data StanGry = WTrakcieGry | Wypadek | Koniec
               deriving Eq

data StanSceny = StanSceny 
                 { sceneScore :: Int
                 , keyPressed :: Bool
                 , leftKeyPressed :: Bool
                 , rightKeyPressed :: Bool
                 , bumpScore  :: Bool
                 , gameState  :: StanGry
                 }

initialSceneState 
  = StanSceny 
    { sceneScore = 0
    , keyPressed = False 
    , leftKeyPressed = False 
    , rightKeyPressed = False 
    , bumpScore  = False 
    , gameState  = WTrakcieGry
    }

type LambdaScene = Scene StanSceny StanObiektu
