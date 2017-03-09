module HUnitTests where
  
import Test.HUnit
import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState
import Obstacles
import Scenery
import Main


isWorldTest1 = TestCase (assertEqual "sampleWorld is a World" True (isWorld sampleWorld))

isScoreTest1 = TestCase (assertEqual "sampleWorld is not a Score" False (isScore sampleWorld))
isScoreTest2 = TestCase (assertEqual "sampleScore is a Score" True (isScore sampleScore))

isHeroTest1 = TestCase (assertEqual "sampleRocket is a Rocket" True (isHero sampleRocket))
                     
tests = TestList [TestLabel "test1" isWorldTest1, TestLabel "test2" isScoreTest1, TestLabel "test2" isScoreTest2,TestLabel "test2" isHeroTest1]

-- | Mock objects definition:

-- | Sample World
sampleWorld :: LambdaNode
sampleWorld = (node [])
                { nodePosition    = Point 0 (sawHeight / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (sawHeight / 2))
                                                  (Point sceneWidth (sawHeight / 2)))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }
           

-- | Sample Rocket
sampleRocket :: LambdaNode
sampleRocket = (spriteWithTexture rocket1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (sceneWidth * 0.5) (sceneHeight * 0.6)
       , nodeActionDirectives = [playAcionInLoop animujBohatera]
       , nodeZRotation        = 0
       , nodePhysicsBody      
           = Just $                
               (bodyWithTextureSize rocket1Texture Nothing (Size (rocketWidth) (rocketHeight))) 
               { bodyCategoryBitMask    = categoryBitMask [Rocket]
               , bodyCollisionBitMask   = categoryBitMask [World]
               , bodyContactTestBitMask = categoryBitMask [World, Score]
               }
       }
  where
    animujBohatera = animateWithTextures 
             [rocket1Texture, rocket2Texture, rocket3Texture, rocket2Texture] 0.1
             
-- | Sample Score
sampleScore :: LambdaNode
sampleScore = (node [])
                { nodePosition    = Point 0 (sawHeight / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (sawHeight / 2))
                                                  (Point sceneWidth (sawHeight / 2)))
                    { bodyCategoryBitMask = categoryBitMask [Score] }
                }

