module Sceneria where
  
import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry
  

(groundTexture, groundTileWidth, groundTileHeight) = stworzTeksture "Land.png"

groundSprites :: [LambdaNode]
groundSprites = [ (spriteWithTexture groundTexture)
                  { nodePosition         = Point x (groundTileHeight / 2)
                  , nodeActionDirectives = [groundMovement]
                  }
                | x <- [0, groundTileWidth..szerokoscSceny + groundTileWidth]]
  where
    movementDuration = 0.005 * groundTileWidth
    groundMovement   = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector (-groundTileWidth) 0)  -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector groundTileWidth 0)     -- reset
                         { actionDuration = 0 }
                       ]

(skyTexture, skyTileWidth, skyTileHeight) = stworzTeksture "spaceBG.jpg"

skySprites :: [LambdaNode]
skySprites = [ (spriteWithTexture skyTexture)
               { nodePosition         = Point (skyWidth/2) y
               , nodeZPosition        = -20
               , nodeXScale           = 4.5
               , nodeYScale           = 4.5
               , nodeActionDirectives = [skyMovement]
               }
             | y <- [0, 4.5*skyTileHeight..4.5*(wysokoscSceny + skyTileHeight)]]
  where
    skyWidth        = szerokoscSceny --(skyTileWidth / 2) + groundTileHeight
    movementDuration = 0.015 * skyTileHeight
    skyMovement      = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector 0 (-4.5*skyTileHeight))   -- move
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector 0 (4.5*skyTileHeight))      -- reset
                         { actionDuration = 0 }
                       ]

