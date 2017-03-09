{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState
import Obstacles
import Scenery


rocketFlying :: LambdaScene

-- | Main game scene
rocketFlying
  = (sceneWithSize (Size sceneWidth sceneHeight))
    { sceneChildren         = [rocket, objectsInMove, sawPhysics, score]
    , sceneData             = initialSceneState
    , sceneUpdate           = Just update
    , scenePhysicsWorld     = physicsWorld
                              { worldGravity         = Vector 0 (-5)
                              , worldContactDidBegin = Just contact
                              }
    , sceneHandleEvent      = Just handleEvent
    }

(rocket1Texture, rocketWidth, rocketHeight)           = createTexture "Rocket-01-T.png"
(rocket2Texture, _, _)                                = createTexture "Rocket-02-T.png"
(rocket3Texture, _, _)                                = createTexture "Rocket-03-T.png"

-- | Rocket as physical object created based on the texture, 
-- | placed in the proper position, animated using defined textures
rocket :: LambdaNode
rocket = (spriteWithTexture rocket1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (sceneWidth * 0.5) (sceneHeight * 0.6)
       , nodeActionDirectives = [playAcionInLoop animateRocket]
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
    animateRocket = animateWithTextures 
             [rocket1Texture, rocket2Texture, rocket3Texture, rocket2Texture] 0.1 
             
-- | List of objects moving through the scene
objectsInMove :: LambdaNode
objectsInMove = (node $ obstacles : saw ++ sky)
              { nodeName = Just "ObjectsInMove" }

-- | Saw physics properties
sawPhysics :: LambdaNode
sawPhysics = (node [])
                { nodePosition    = Point 0 (sawHeight / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (sawHeight / 2))
                                                  (Point sceneWidth (sawHeight / 2)))
                    { bodyCategoryBitMask = categoryBitMask [World] }
                }

-- | Creates and moves obstacles through the scene
obstacles :: LambdaNode
obstacles = (node [])
        { nodeActionDirectives = [playSequenceOfActionsInLoop 
                                  [ customAction (placePairOfObstacles rocketHeight)
                                  , waitForDuration{ actionDuration = 3 }
                                  ] ]
        , nodeUserData         = ObstaclesState getRandomNumber 
        }

-- | Field with the current player's score
score :: LambdaNode
score = (labelNodeWithFontNamed "Verdana")
        { nodeName      = Just "Score"
        , nodePosition  = Point (sceneWidth / 2) (4 * sceneHeight / 5)
        , nodeZPosition = 100
        , nodeXScale = 3
        , nodeYScale = 3
        , labelText     = "0"
        }

-- | Updates the scene based on actual game state in case of events
update :: LambdaScene -> TimeInterval -> LambdaScene
update scene@Scene{ sceneData = sceneState@SceneState{..} } _dt 
  = case gameState of
      InGame 
        | keyPressed -> accelerateRocket scene{ sceneData = sceneState{ keyPressed = False } }
        | leftKeyPressed -> rocketTurn scene{ sceneData = sceneState{ leftKeyPressed = False } } True
        | rightKeyPressed -> rocketTurn scene{ sceneData = sceneState{ rightKeyPressed = False } } False
        | bumpScore  -> incScore scene{ sceneData = sceneState{ bumpScore = False } }
      Crash        -> crash scene{ sceneData = sceneState{ gameState = End } }
      End         -> scene

-- | Accelerates Rocket
accelerateRocket :: LambdaScene -> LambdaScene
accelerateRocket scene
  = scene { sceneActionDirectives = [playCustomActionOn "Lambda" actionJump] }
  
-- | Trun the rocket
rocketTurn :: LambdaScene -> Bool -> LambdaScene
rocketTurn scene isLeftTurn
  = scene { sceneActionDirectives = [playCustomActionOn "Lambda" (actionTurn isLeftTurn)] }

-- | When Rocket crashes into other object
crash :: LambdaScene -> LambdaScene
crash scene
  = scene { sceneActionDirectives = [ playActionOn "Lambda" crashAction
                                    , playActionOn "ObjectsInMove" stopMoving
                                    ] }
  where
    crashAction = sequenceActions
                  [ (rotateByAngle (-pi * 4)){ actionDuration = 1 }
                  , fadeOut{ actionDuration = 0.2 }
                  ]
    stopMoving  = sequenceActions
                  [ waitForDuration{ actionDuration = 1 }
                  , customAction $ \node _ -> node{ nodeSpeed = 0 }
                  ] 
    
incScore :: LambdaScene -> LambdaScene 
incScore scene@Scene{ sceneData = sceneState }
  = scene
    { sceneActionDirectives = [playCustomActionOn "Score" setScore]
    , sceneData             = sceneState{ sceneScore = newScore }
    }
  where
    newScore = sceneScore sceneState + 1

    setScore label@Label{} _dt = label{ labelText = show newScore }
    setScore node          _   = node

contact :: SceneState 
        -> PhysicsContact u
        -> (Maybe SceneState, Maybe (Node u), Maybe (Node u))
contact state@SceneState{..} PhysicsContact{..}
  | (isWorld contactBodyA || isWorld contactBodyB) && gameState == InGame
  = (Just state{ gameState = Crash }, Nothing, Nothing)
  | isScore contactBodyA || isScore contactBodyB
  = (Just state{ bumpScore = True }, Nothing, Nothing)
  | otherwise
  = (Nothing, Nothing, Nothing)  

-- | Events handlers (key pressed)
handleEvent :: Event -> SceneState -> Maybe SceneState
handleEvent KeyEvent{ keyEventType = KeyDown } state = Just state{ keyPressed = True }
handleEvent MouseEvent{ mouseEventType = LeftMouseDown } state = Just state{ leftKeyPressed = True }
handleEvent MouseEvent{ mouseEventType = RightMouseDown } state = Just state{ rightKeyPressed = True }
handleEvent _ _     = Nothing
