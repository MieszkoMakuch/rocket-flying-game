module TestyHUnit where
  
import Test.HUnit
import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry
import Przeszkody
import LosoweLiczby
import Sceneria
import Main


jestSwiatemTest1 = TestCase (assertEqual "przykladowySwiat jest swiatem" True (jestSwiatem przykladowySwiat))

jestWynikiemTest1 = TestCase (assertEqual "przykladowySwiat nie jest wynikiem" False (jestWynikiem przykladowySwiat))
jestWynikiemTest2 = TestCase (assertEqual "przykladowyWynik jest wynikiem" True (jestWynikiem przykladowyWynik))

jestBohateremTest1 = TestCase (assertEqual "przykladowyBohater jest bohaterem" True (jestBohaterem przykladowyBohater))
                     
tests = TestList [TestLabel "test1" jestSwiatemTest1, TestLabel "test2" jestWynikiemTest1, TestLabel "test2" jestWynikiemTest2,TestLabel "test2" jestBohateremTest1]

-- | Definicje Mock objeków:

-- | Przykladowy LambdaNode typu Swiat
przykladowySwiat :: LambdaNode
przykladowySwiat = (node [])
                { nodePosition    = Point 0 (wysokoscOstrzy / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (wysokoscOstrzy / 2))
                                                  (Point szerokoscSceny (wysokoscOstrzy / 2)))
                    { bodyCategoryBitMask = categoryBitMask [Swiat] }
                }
           

-- | Przykladowy Bohater
przykladowyBohater :: LambdaNode
przykladowyBohater = (spriteWithTexture rocket1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (szerokoscSceny * 0.5) (wysokoscSceny * 0.6)
       , nodeActionDirectives = [odtwarzajAkcjeWNieskonczonosc animujBohatera]
       , nodeZRotation        = 0
       , nodePhysicsBody      
           = Just $                
               (bodyWithTextureSize rocket1Texture Nothing (Size (szerokoscBohatera) (wysokoscBohatera))) 
               { bodyCategoryBitMask    = categoryBitMask [Bohater]
               , bodyCollisionBitMask   = categoryBitMask [Swiat]
               , bodyContactTestBitMask = categoryBitMask [Swiat, Wynik]
               }
       }
  where
    animujBohatera = animateWithTextures --Definicja akcji flap - machanie skrzydłami
             [rocket1Texture, rocket2Texture, rocket3Texture, rocket2Texture] 0.1 --[ zdjecie 1, zdjecie 2, zdjecie 3, zdjecie 2] co x sekund
             
-- | Przykladowy LambdaNode typu Wynik
przykladowyWynik :: LambdaNode
przykladowyWynik = (node [])
                { nodePosition    = Point 0 (wysokoscOstrzy / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (wysokoscOstrzy / 2))
                                                  (Point szerokoscSceny (wysokoscOstrzy / 2)))
                    { bodyCategoryBitMask = categoryBitMask [Wynik] }
                }

