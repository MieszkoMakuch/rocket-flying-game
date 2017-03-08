module GameState where

import Graphics.SpriteKit

  
data ObjectState = ObstaclesState [Int]
               | NoState

type LambdaNode = Node ObjectState

randomInt :: ObjectState -> (ObjectState, Int)
randomInt NoState             = (NoState, 0)
randomInt (ObstaclesState (i:is)) = (ObstaclesState is, i)


data GameState = InGame | Crash | End
                 deriving Eq

data SceneState = SceneState 
                 { sceneScore :: Int
                 , keyPressed :: Bool
                 , leftKeyPressed :: Bool
                 , rightKeyPressed :: Bool
                 , bumpScore  :: Bool
                 , gameState  :: GameState
                 }

initialSceneState 
  = SceneState 
    { sceneScore = 0
    , keyPressed = False 
    , leftKeyPressed = False 
    , rightKeyPressed = False 
    , bumpScore  = False 
    , gameState  = InGame
    }

type LambdaScene = Scene SceneState ObjectState
