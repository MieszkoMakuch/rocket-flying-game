module Akcje where

import Graphics.SpriteKit

import StanGry
import Stale


-- | Podskok przy nacisnieciu klawisza
akcjaPodskok :: LambdaNode -> TimeInterval -> LambdaNode
akcjaPodskok sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody
        = Just body
               {bodyVelocity          = vectorZero, -- Prędkość równa 0 (lub Vector 0 0)
                bodyForcesAndImpulses = [ApplyImpulse (Vector 0 (2*silaSkretu)) Nothing] --podnosi o x do góry
               }
    }
bumpAction node _dt = node

-- | Skręt przy nacisnieciu klawisza
akcjaLewySkret :: LambdaNode -> TimeInterval -> LambdaNode
akcjaLewySkret sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody
        = Just body
               { --bodyVelocity          = Vector 0 0,  -- Prędkość równa 0 (lub Vector 0 0)
                bodyForcesAndImpulses = [ApplyImpulse (Vector (-silaSkretu) 0) Nothing] --podnosi o x do góry
               }
    }
    
-- | Skręt przy nacisnieciu klawisza
akcjaPrawySkret :: LambdaNode -> TimeInterval -> LambdaNode
akcjaPrawySkret sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody
        = Just body
               { --bodyVelocity          = vectorZero, -- Prędkość równa 0 (lub Vector 0 0)
                bodyForcesAndImpulses = [ApplyImpulse (Vector (silaSkretu) 0) Nothing] --podnosi o x do góry
               }
    }

