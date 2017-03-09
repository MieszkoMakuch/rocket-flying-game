module Obstacles where
  
import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState


(rightObstacleTexture, rightObstacleWidth, rightObstacleHeight) = createTexture "stone1.png"
(leftObstacleTexture, leftObstacleWidth, leftObstacleHeight) = createTexture "stone2.png"

placePairOfObstacles :: GFloat -> LambdaNode -> TimeInterval -> LambdaNode
placePairOfObstacles rocketWidth pipes _dt
  = pipes
    { nodeChildren = createPairOfObstacles rocketWidth rightObstacleXPosition : nodeChildren pipes 
    , nodeUserData = pipeState'
    }
  where
    maxGap = round (1.5*sceneWidth) -- max gap between two obstacles
    (pipeState', i) = randomInt (nodeUserData pipes)
    rightObstacleXPosition = fromIntegral $ 0 + (i `mod` maxGap) `div` 2

-- Returns a pair of obstacles with the gap between as a list of objects (rightObstacle, gap, rightObstacle). 
createPairOfObstacles :: GFloat -> GFloat -> LambdaNode
createPairOfObstacles rocketWidth rightObstacleXPosition
  = (node [leftObstacle rightObstacleXPosition, gap rocketWidth, rightObstacle rightObstacleXPosition])
    { nodePosition         = Point 0 (sceneHeight + rightObstacleHeight) 
    , nodeZPosition        = -10
    , nodeActionDirectives = [moveAndRemoveObstacles]
    }
  where
    moveAndRemoveObstacles  = runAction $ 
                           sequenceActions [ (moveBy $ Vector 0 (-moveByDist)) 
                                             { actionDuration = 0.005 * moveByDist } 
                                           , removeFromParent 
                                           ]
    moveByDist              = sceneHeight + (2*rightObstacleHeight)
    leftObstacle rightObstacleXPosition = createObstacle leftObstacleTexture 
                              (rightObstacleXPosition + leftObstacleWidth + gapSize)
                              leftObstacleWidth
                              leftObstacleHeight
    rightObstacle rightObstacleXPosition = createObstacle rightObstacleTexture 
                              rightObstacleXPosition
                              rightObstacleWidth
                              rightObstacleHeight

-- Returns obstacle with specified: (texture, y position, width, height)
createObstacle :: Texture -> GFloat -> GFloat -> GFloat -> LambdaNode
createObstacle texture x width height
  = (spriteWithTexture texture) 
    { nodePosition    = Point x 300
    , nodePhysicsBody = Just $ (bodyWithTextureSize texture Nothing (Size (width) (height))) 
                               { bodyIsDynamic          = False
                               , bodyCategoryBitMask    = categoryBitMask [World] 
                               , bodyContactTestBitMask = categoryBitMask [Rocket]
                               }
    }

gap :: GFloat -> LambdaNode
gap rocketWidth
  = (node [])
    { nodePosition    = Point (sceneWidth / 2) (leftObstacleHeight / 2 + rocketWidth / 2) 
    , nodePhysicsBody = Just $ (bodyWithEdgeFromPointToPoint (Point 0 0) (Point sceneWidth 0))
                               { bodyCategoryBitMask    = categoryBitMask [Score]
                               , bodyContactTestBitMask = categoryBitMask [Rocket]
                               }
    }
