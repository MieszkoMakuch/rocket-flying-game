module Przeszkody where
  
import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry


(teksturaGornejPrzeszkody, szerokoscGornejPrzeszkody, wysokoscGornejPrzeszkody) = stworzTeksture "PipeUp.png"
(teksturaDolnejPrzeszkody, szerokoscDolnejPrzeszkody, wysokoscDolnejPrzeszkody) = stworzTeksture "PipeDown.png"

umiescParePrzeskod :: GFloat -> LambdaNode -> TimeInterval -> LambdaNode
umiescParePrzeskod wysokoscBohatera pipes _dt
  = pipes
    { nodeChildren = stworzParePrzeszkod wysokoscBohatera pipeUpY : nodeChildren pipes 
    , nodeUserData = pipeState'
    }
  where
    maksymalneOdchylenie = round (wysokoscSceny / 1.5) -- maksymalna różnica między pozycjami przeszkód 
    (pipeState', i) = randomInt (nodeUserData pipes)
    pipeUpY         = fromIntegral $ 120 + (i `mod` maksymalneOdchylenie) `div` 2 --losowa pozycja górnej przeszkody

-- Zwraca parę przeszkód ze szczeliną pomiędzy jako lista obiektów (przeszkodaDolna, szczelina, przeszkodaGorna)
-- Przyjmuje wysokość bohatera i pozycje górnej przeszkody
stworzParePrzeszkod :: GFloat -> GFloat -> LambdaNode
stworzParePrzeszkod wysokoscBohatera przeszkodaGornaY
  = (node [przeszkodaDolna przeszkodaGornaY, szczelina wysokoscBohatera, przeszkodaGorna przeszkodaGornaY])
    { nodePosition         = Point (szerokoscSceny + szerokoscGornejPrzeszkody) 0
    , nodeZPosition        = -10
    , nodeActionDirectives = [przesuwajPrzeszkodyIUsunJe]
    }
  where
    przesuwajPrzeszkodyIUsunJe = runAction $ --przesuwa przeszkody przez scene, po opuszczeniu sceny usuwa przeszkody
                           sequenceActions [ (moveBy $ Vector (-oIlePrzesunac) 0) --o ile przesunąć przeszkody
                                             { actionDuration = 0.005 * oIlePrzesunac } --czas trwania akcji (domyślnie 0.005 * oIlePrzesunac)
                                           , removeFromParent --usun przeszkody ze sceny
                                           ]
    oIlePrzesunac     = szerokoscSceny + szerokoscGornejPrzeszkody
    przeszkodaDolna przeszkodaGornaY   = stworzPrzeszkode teksturaDolnejPrzeszkody 
                              (przeszkodaGornaY + wysokoscDolnejPrzeszkody + wysokoscSzczeliny)
                              szerokoscDolnejPrzeszkody
                              wysokoscDolnejPrzeszkody
    przeszkodaGorna przeszkodaGornaY     = stworzPrzeszkode teksturaGornejPrzeszkody 
                              przeszkodaGornaY
                              szerokoscGornejPrzeszkody
                              wysokoscGornejPrzeszkody

-- Zwraca przeszkodę o określonej (teksturze, pozycji y, szerokości, wysokości)
stworzPrzeszkode :: Texture -> GFloat -> GFloat -> GFloat -> LambdaNode
stworzPrzeszkode texture y width height
  = (spriteWithTexture texture) --dwuwymiarowy obrazek z teksturą
    { nodePosition    = Point 0 y --pozycja przeszkody
    , nodePhysicsBody = Just $ (bodyWithRectangleOfSize (Size width height) Nothing) --nowy obiekt prostokątny o wymiarach width, height
                               { bodyIsDynamic          = False
                               , bodyCategoryBitMask    = categoryBitMask [Swiat] --przypisanie kategorii BitMask
                               , bodyContactTestBitMask = categoryBitMask [Bohater]  --przypisanie kontaktu z kategorią BitMask
                               }
    }

-- Liczy punkt jeśli 
szczelina :: GFloat -> LambdaNode
szczelina wysokoscBohatera
  = (node [])
    { nodePosition    = Point (szerokoscDolnejPrzeszkody / 2 + wysokoscBohatera / 2) (wysokoscSceny / 2)
    , nodePhysicsBody = Just $ (bodyWithEdgeFromPointToPoint (Point 0 0) (Point 0 wysokoscSceny))
                               { bodyCategoryBitMask    = categoryBitMask [Wynik]
                               , bodyContactTestBitMask = categoryBitMask [Bohater]
                               }
    }
