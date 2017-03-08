module DefinedConstants where

import Graphics.SpriteKit

import Data.Bits
import Data.Word


-- | Scene size
sceneWidth, sceneHeight :: GFloat
sceneWidth  = 850
sceneHeight = 1200

-- | Size of the gap between obstacles
gapSize :: GFloat
gapSize = 150

-- | Categorisation of physical objects (as Enum)
data PhysicalObjects = Rocket    
                     | World      -- Obstacles and ground
                     | Score      -- Scoring nodes
                     deriving (Enum)

categoryBitMask :: [PhysicalObjects] -> Word32
categoryBitMask = foldl setCategoryBit zeroBits
  where
    setCategoryBit bits cat = bits .|. bit (fromEnum cat)

isPhysicalObject :: PhysicalObjects -> Node u -> Bool
isPhysicalObject object node
  = case nodePhysicsBody node of
      Just body -> testBit (bodyCategoryBitMask body) (fromEnum object)
      Nothing   -> False

isWorld :: Node u -> Bool
isWorld = isPhysicalObject World

isScore :: Node u -> Bool
isScore = isPhysicalObject Score

isHero :: Node u -> Bool
isHero = isPhysicalObject Rocket

-- | Steering force (left/right turn)
steeringForce :: Double
steeringForce = 60.0
