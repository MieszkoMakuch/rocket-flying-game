module Scenery where
  
import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState
  

(sawTexture, sawWidth, sawHeight) = createTexture "saw.png"

saw :: [LambdaNode]
saw = [ (spriteWithTexture sawTexture)
                  { nodePosition         = Point x (sawHeight / 2)
                  , nodeXScale           = 2
                  , nodeYScale           = 2
                  , nodeActionDirectives = [sawMovement]
                  }
                | x <- [0, 2*sawWidth..2*sceneWidth + 2*sawWidth]]
  where
    movementDuration = 0.009 * sawWidth
    sawMovement   = playSequenceOfActionsInLoop
                       [ (moveBy $ Vector (-2*sawWidth) 0)  --move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector (2*sawWidth) 0)   --reset
                         { actionDuration = 0 }
                       ]

(spaceTexture, spaceWidth, spaceHeight) = createTexture "spaceBG.jpg"

sky :: [LambdaNode]
sky = [ (spriteWithTexture spaceTexture)
               { nodePosition         = Point (sceneWidth/2) y
               , nodeZPosition        = -20
               , nodeXScale           = 1.1
               , nodeYScale           = 1.1
               , nodeActionDirectives = [spaceMovement]
               }
             | y <- [0, 1.1*spaceHeight..1.1*(sceneHeight + spaceHeight)]]
  where
    movementInterval = 0.045 * spaceHeight
    spaceMovement      = playSequenceOfActionsInLoop
                       [ (moveBy $ Vector 0 (-1.1*spaceHeight)) --move
                         { actionDuration = movementInterval }
                       , (moveBy $ Vector 0 (1.1*spaceHeight))  --reset
                         { actionDuration = 0 }
                       ]

