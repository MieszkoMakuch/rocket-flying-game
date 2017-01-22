{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Graphics.SpriteKit

import Akcje
import Stale
import FunkcjePomocnicze
import StanGry
import Przeszkody
import LosoweLiczby
import Sceneria


lazyLambda :: LambdaScene

lazyLambda
  = (sceneWithSize (Size szerokoscSceny wysokoscSceny))
    { sceneBackgroundColor  = kolorNieba
    , sceneChildren         = [bird, movingNodes, groundPhysics, score]
    , sceneData             = initialSceneState
    , sceneUpdate           = Just update
    , scenePhysicsWorld     = physicsWorld
                              { worldGravity         = Vector 0 (-5)
                              , worldContactDidBegin = Just contact
                              }
    , sceneHandleEvent      = Just handleEvent
    }

(bird1Texture, szerokoscBohatera, wysokoscBohatera) = stworzTeksture "Bird-01.png"
(bird2Texture, _, _)                  = stworzTeksture "Bird-02.png"
(bird3Texture, _, _)                  = stworzTeksture "Bird-03.png"

bird :: LambdaNode
bird = (spriteWithTexture bird1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (szerokoscSceny * 0.35) (wysokoscSceny * 0.6) --w którym miejscu będzie ptak na starcie
       , nodeActionDirectives = [odtwarzajAkcjeWNieskonczonosc flap] --powtarzaj akcje flap (machanie skrzydłami) w nieskończoność (definicja flap poniżej)
       , nodeZRotation        = 0.003 --rotacja o x radianów wzgl. osi z (1 radian około 60stopni)
       , nodePhysicsBody      
           = Just $
               (bodyWithCircleOfRadius (wysokoscBohatera / 2) Nothing) --tworzy kołowy obiekt fizyczny o poromieniu r (obserwacja: im mniejsze r tym mniejsza masa)
               { bodyCategoryBitMask    = categoryBitMask [Bohater] --[Bohater] - należy do Enuma zdefiniowanego w Constants
               , bodyCollisionBitMask   = categoryBitMask [Swiat]
               , bodyContactTestBitMask = categoryBitMask [Swiat, Wynik]
               }
       }
  where
    flap = animateWithTextures --Definicja akcji flap - machanie skrzydłami
             [bird1Texture, bird2Texture, bird3Texture, bird2Texture] 0.1 --[ zdjecie 1, zdjecie 2, zdjecie 3, zdjecie 2] co x sekund

movingNodes :: LambdaNode
movingNodes = (node $ pipes : groundSprites ++ skySprites)
              { nodeName = Just "Moving" }

groundPhysics :: LambdaNode
groundPhysics = (node [])
                { nodePosition    = Point 0 (groundTileHeight / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (groundTileHeight / 2))
                                                  (Point szerokoscSceny (groundTileHeight / 2)))
                    { bodyCategoryBitMask = categoryBitMask [Swiat] }
                }

pipes :: LambdaNode
pipes = (node [])
        { nodeActionDirectives = [odtwarzajListeAkcjiWNieskonczonosc 
                                  [ customAction (umiescParePrzeskod szerokoscBohatera)
                                  , waitForDuration{ actionDuration = 1.5 } --w sekundach
                                  ] ]
        , nodeUserData         = PipesState generujLosowaLiczbe 
        }

score :: LambdaNode
score = (labelNodeWithFontNamed "MarkerFelt-Wide")
        { nodeName      = Just "Score"
        , nodePosition  = Point (szerokoscSceny / 2) (3 * wysokoscSceny / 4)
        , nodeZPosition = 100
        , labelText     = "0"
        }

update :: LambdaScene -> TimeInterval -> LambdaScene
update scene@Scene{ sceneData = sceneState@SceneState{..} } _dt 
  = case gameState of
      Running 
        | keyPressed -> bumpLambda scene{ sceneData = sceneState{ keyPressed = False } }
        | bumpScore  -> incScore scene{ sceneData = sceneState{ bumpScore = False } }
        | otherwise  -> tiltLambda scene
      Crash          -> crash scene{ sceneData = sceneState{ gameState = Over } }
      Over           -> scene
  
bumpLambda :: LambdaScene -> LambdaScene
bumpLambda scene
  = scene { sceneActionDirectives = [odtworzWlasnaAkcjeNa "Lambda" akcjaPodskok] }

tiltLambda :: LambdaScene -> LambdaScene
tiltLambda scene 
  = scene{ sceneActionDirectives = [odtworzWlasnaAkcjeNa "Lambda" akcjaPrzechyl] }

crash :: LambdaScene -> LambdaScene
crash scene
  = scene { sceneActionDirectives = [ odtworzAkcjeNa "Lambda" crashAction
                                    , odtworzAkcjeNa "Moving" stopMoving
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
    { sceneActionDirectives = [odtworzWlasnaAkcjeNa "Score" setScore]
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
  | (jestSwiatem contactBodyA || jestSwiatem contactBodyB) && gameState == Running
  = (Just state{ gameState = Crash }, Nothing, Nothing)
  | jestWynikiem contactBodyA || jestWynikiem contactBodyB
  = (Just state{ bumpScore = True }, Nothing, Nothing)
  | otherwise
  = (Nothing, Nothing, Nothing)  

-- 
handleEvent :: Event -> SceneState -> Maybe SceneState
handleEvent KeyEvent{ keyEventType = KeyDown } state = Just state{ keyPressed = True }
handleEvent _                                  _     = Nothing
