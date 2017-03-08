module Scenery where
  
import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState
  

(teksturaOstrzy, szerokoscOstrzy, wysokoscOstrzy) = stworzTeksture "saw.png"

ostrza :: [LambdaNode]
ostrza = [ (spriteWithTexture teksturaOstrzy)
                  { nodePosition         = Point x (wysokoscOstrzy / 2)
                  , nodeXScale           = 2
                  , nodeYScale           = 2
                  , nodeActionDirectives = [ruchOstrzy]
                  }
                | x <- [0, 2*szerokoscOstrzy..2*sceneWidth + 2*szerokoscOstrzy]]
  where
    movementDuration = 0.009 * szerokoscOstrzy
    ruchOstrzy   = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector (-2*szerokoscOstrzy) 0)  --przenieś
                         { actionDuration = movementDuration }
                       , (moveBy $ Vector (2*szerokoscOstrzy) 0)   --zresetuj
                         { actionDuration = 0 }
                       ]

(teksturaKosmosu, szerokoscKosmosu, wysokoscKosmosu) = stworzTeksture "spaceBG.jpg"

kosmos :: [LambdaNode]
kosmos = [ (spriteWithTexture teksturaKosmosu)
               { nodePosition         = Point (sceneWidth/2) y
               , nodeZPosition        = -20
               , nodeXScale           = 1.1
               , nodeYScale           = 1.1
               , nodeActionDirectives = [ruchKosmosu]
               }
             | y <- [0, 1.1*wysokoscKosmosu..1.1*(sceneHeight + wysokoscKosmosu)]]
  where
    czasRuchu = 0.045 * wysokoscKosmosu
    ruchKosmosu      = odtwarzajListeAkcjiWNieskonczonosc
                       [ (moveBy $ Vector 0 (-1.1*wysokoscKosmosu)) --przenieś
                         { actionDuration = czasRuchu }
                       , (moveBy $ Vector 0 (1.1*wysokoscKosmosu))  --zresetuj
                         { actionDuration = 0 }
                       ]

