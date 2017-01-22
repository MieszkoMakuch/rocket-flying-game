module Sceneria where
  
import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry
  

(groundTexture, groundTileWidth, groundTileHeight) = stworzTeksture "saw.png"

groundSprites :: [LambdaNode]
groundSprites = [ (spriteWithTexture groundTexture)
                  { nodePosition         = Point x (groundTileHeight / 2)
                  , nodeXScale           = 2--4.5
                  , nodeYScale           = 2--4.5
                  , nodeActionDirectives = [groundMovement]
                  }
                | x <- [0, 2*groundTileWidth..2*szerokoscSceny + 2*groundTileWidth]]
  where
    movementDuration = 0.005 * groundTileWidth
    groundMovement   = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector (-2*groundTileWidth) 0)  -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector (2*groundTileWidth) 0)     -- reset
                         { actionDuration = 0 }
                       ]

(skyTexture, skyTileWidth, skyTileHeight) = stworzTeksture "spaceBG.jpg"

skySprites :: [LambdaNode]
skySprites = [ (spriteWithTexture skyTexture)
               { nodePosition         = Point (skyWidth/2) y
               , nodeZPosition        = -20
               , nodeXScale           = 1.1--4.5
               , nodeYScale           = 1.1--4.5
               , nodeActionDirectives = [skyMovement]
               }
             | y <- [0, 1.1*skyTileHeight..1.1*(wysokoscSceny + skyTileHeight)]]
  where
    skyWidth        = szerokoscSceny --(skyTileWidth / 2) + groundTileHeight
    movementDuration = 0.045 * skyTileHeight
    skyMovement      = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector 0 (-1.1*skyTileHeight))   -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector 0 (1.1*skyTileHeight))      -- reset
                         { actionDuration = 0 }
                       ]

