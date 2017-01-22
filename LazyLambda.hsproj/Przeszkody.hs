module Przeszkody where
  
import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry


(teksturaPrawejPrzeszkody, szerokoscPrawejPrzeszkody, wysokoscPrawejPrzeszkody) = stworzTeksture "PipeRight.png"
(teksturaLewejPrzeszkody, szerokoscLewejPrzeszkody, wysokoscLewejPrzeszkody) = stworzTeksture "PipeLeft.png"

umiescParePrzeskod :: GFloat -> LambdaNode -> TimeInterval -> LambdaNode
umiescParePrzeskod wysokoscBohatera pipes _dt
  = pipes
    { nodeChildren = stworzParePrzeszkod wysokoscBohatera przeszkodaPrawaX : nodeChildren pipes 
    , nodeUserData = pipeState'
    }
  where
    maksymalneOdchylenie = round (szerokoscSceny / 1.5) -- maksymalna różnica między pozycjami przeszkód 
    (pipeState', i) = randomInt (nodeUserData pipes)
    przeszkodaPrawaX         = fromIntegral $ 0 + (i `mod` maksymalneOdchylenie) `div` 2 --losowa pozycja górnej przeszkody

-- Zwraca parę przeszkód ze szczeliną pomiędzy jako lista obiektów (przeszkodaLewa, szczelina, przeszkodaPrawa)
-- Przyjmuje wysokość bohatera i pozycje górnej przeszkody
stworzParePrzeszkod :: GFloat -> GFloat -> LambdaNode
stworzParePrzeszkod wysokoscBohatera przeszkodaPrawaX
  = (node [przeszkodaLewa przeszkodaPrawaX, szczelina wysokoscBohatera, przeszkodaPrawa przeszkodaPrawaX])
    { nodePosition         = Point 0 (wysokoscSceny + wysokoscPrawejPrzeszkody) 
    , nodeZPosition        = 5 --powinno być -10
    , nodeActionDirectives = [przesuwajPrzeszkodyIUsunJe]
    }
  where
    przesuwajPrzeszkodyIUsunJe = runAction $ --przesuwa przeszkody przez scene, po opuszczeniu sceny usuwa przeszkody
                           sequenceActions [ (moveBy $ Vector 0 (-oIlePrzesunac)) --o ile przesunąć przeszkody
                                             { actionDuration = 0.005 * oIlePrzesunac } --czas trwania akcji (domyślnie 0.005 * oIlePrzesunac)
                                           , removeFromParent --usun przeszkody ze sceny
                                           ]
    oIlePrzesunac     = szerokoscSceny + szerokoscPrawejPrzeszkody
    przeszkodaLewa przeszkodaPrawaX   = stworzPrzeszkode teksturaLewejPrzeszkody 
                              (przeszkodaPrawaX + szerokoscLewejPrzeszkody + szerokoscSzczeliny)
                              szerokoscLewejPrzeszkody
                              wysokoscLewejPrzeszkody
    przeszkodaPrawa przeszkodaPrawaX     = stworzPrzeszkode teksturaPrawejPrzeszkody 
                              przeszkodaPrawaX
                              szerokoscPrawejPrzeszkody
                              wysokoscPrawejPrzeszkody

-- Zwraca przeszkodę o określonej (teksturze, pozycji y, szerokości, wysokości)
stworzPrzeszkode :: Texture -> GFloat -> GFloat -> GFloat -> LambdaNode
stworzPrzeszkode texture x szerokosc wysokosc
  = (spriteWithTexture texture) --dwuwymiarowy obrazek z teksturą
    { nodePosition    = Point x 300 --pozycja przeszkody
    , nodePhysicsBody = Just $ (bodyWithRectangleOfSize (Size szerokosc wysokosc) Nothing) --nowy obiekt prostokątny o wymiarach width, height
                               { bodyIsDynamic          = False
                               , bodyCategoryBitMask    = categoryBitMask [Swiat] --przypisanie kategorii BitMask
                               , bodyContactTestBitMask = categoryBitMask [Bohater]  --przypisanie kontaktu z kategorią BitMask
                               }
    }

-- Liczy punkt jeśli 
szczelina :: GFloat -> LambdaNode
szczelina wysokoscBohatera
  = (node [])
    { nodePosition    = Point (wysokoscSceny / 2) (szerokoscLewejPrzeszkody / 2 + wysokoscBohatera / 2) 
    , nodePhysicsBody = Just $ (bodyWithEdgeFromPointToPoint (Point 0 wysokoscSceny) (Point 0 0))
                               { bodyCategoryBitMask    = categoryBitMask [Wynik]
                               , bodyContactTestBitMask = categoryBitMask [Bohater]
                               }
    }
