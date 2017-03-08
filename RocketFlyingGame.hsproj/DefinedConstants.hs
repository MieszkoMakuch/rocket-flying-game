module DefinedConstants where

import Graphics.SpriteKit

import Data.Bits
import Data.Word


-- | Scene size
sceneWidth, sceneHeight :: GFloat
sceneWidth  = 850
sceneHeight = 1200

-- | Wysokość szczeliny przez którą musi przeleciec bohater
szerokoscSzczeliny :: GFloat
szerokoscSzczeliny = 150

-- | Kategorycazja różnych obiektów świata fizycznego (jako enum)
data ObiektyFizyczne = Bohater    -- Lambda
                     | Swiat      -- Przeszkody i grunt
                     | Wynik      -- Scoring nodes
                     deriving (Enum)

categoryBitMask :: [ObiektyFizyczne] -> Word32
categoryBitMask = foldl setCategoryBit zeroBits
  where
    setCategoryBit bits cat = bits .|. bit (fromEnum cat)

isPhysicalObject :: ObiektyFizyczne -> Node u -> Bool
isPhysicalObject obiekt node
  = case nodePhysicsBody node of
      Just body -> testBit (bodyCategoryBitMask body) (fromEnum obiekt)
      Nothing   -> False

isWorld :: Node u -> Bool
isWorld = isPhysicalObject Swiat

isScore :: Node u -> Bool
isScore = isPhysicalObject Wynik

isHero :: Node u -> Bool
isHero = isPhysicalObject Bohater

-- | Steering force
steeringForce :: Double
steeringForce = 60.0
