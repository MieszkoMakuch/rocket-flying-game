module DefinedConstants where

import Graphics.SpriteKit

import Data.Bits
import Data.Word


-- | Wymiary sceny
szerokoscSceny, wysokoscSceny :: GFloat
szerokoscSceny  = 850
wysokoscSceny = 1200

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

jestObiektemFizycznym :: ObiektyFizyczne -> Node u -> Bool
jestObiektemFizycznym obiekt node
  = case nodePhysicsBody node of
      Just body -> testBit (bodyCategoryBitMask body) (fromEnum obiekt)
      Nothing   -> False

jestSwiatem :: Node u -> Bool
jestSwiatem = jestObiektemFizycznym Swiat

jestWynikiem :: Node u -> Bool
jestWynikiem = jestObiektemFizycznym Wynik

jestBohaterem :: Node u -> Bool
jestBohaterem = jestObiektemFizycznym Bohater

-- | Kolor nieba
kolorNieba :: Color
kolorNieba = colorWithRGBA (81.0/255.0)
                          (192.0/255.0)
                          (201.0/255.0)
                          1.0
-- | Siła skrętu
silaSkretu :: Double
silaSkretu = 60
