module Actions where

import Graphics.SpriteKit

import GameState
import DefinedConstants


-- | Jump on key press
actionJump :: LambdaNode -> TimeInterval -> LambdaNode
actionJump sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody
        = Just body
               {bodyVelocity          = vectorZero,
                bodyForcesAndImpulses = [ApplyImpulse (Vector 0 (2*steeringForce)) Nothing]
               }
    }
bumpAction node _dt = node

-- | Turn on key press
actionTurn :: Bool -> LambdaNode -> TimeInterval -> LambdaNode
actionTurn isLeftTurn sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody
        = Just body
               {
                bodyForcesAndImpulses = [ApplyImpulse (Vector (
                  if isLeftTurn then -steeringForce else steeringForce) 
                  0) Nothing] 
               }
    }

