{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Graphics.SpriteKit

import Actions
import DefinedConstants
import HelperFunctions
import GameState
import Obstacles
import Scenery


rocketFlying :: LambdaScene

-- | Główna scena gry
rocketFlying
  = (sceneWithSize (Size sceneWidth sceneHeight))
    { sceneChildren         = [rakieta, poruszajaceSieObiekty, fizykaOstrzy, wynik]
    , sceneData             = initialSceneState
    , sceneUpdate           = Just update
    , scenePhysicsWorld     = physicsWorld
                              { worldGravity         = Vector 0 (-5)
                              , worldContactDidBegin = Just contact
                              }
    , sceneHandleEvent      = Just handleEvent
    }

(rocket1Texture, szerokoscBohatera, wysokoscBohatera) = stworzTeksture "Rocket-01-T.png"
(rocket2Texture, _, _)                                = stworzTeksture "Rocket-02-T.png"
(rocket3Texture, _, _)                                = stworzTeksture "Rocket-03-T.png"

-- | Obiekt fizyczny rakieta stworzony na podstawie tekstury, ustawiony w odpowienim miejscu, 
-- | animowany z wykorzystaniem zdefiniowanych tekstur
rakieta :: LambdaNode
rakieta = (spriteWithTexture rocket1Texture)
       { nodeName             = Just "Lambda"
       , nodePosition         = Point (sceneWidth * 0.5) (sceneHeight * 0.6)
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

-- | Lista poruszających się po scenie obiektów 
poruszajaceSieObiekty :: LambdaNode
poruszajaceSieObiekty = (node $ przeszkody : ostrza ++ kosmos)
              { nodeName = Just "ObiektyWRuchu" }

-- | Właściwości fizyczne opisujące ostrza
fizykaOstrzy :: LambdaNode
fizykaOstrzy = (node [])
                { nodePosition    = Point 0 (wysokoscOstrzy / 2)
                , nodePhysicsBody = Just $
                    (bodyWithEdgeFromPointToPoint (Point 0 (wysokoscOstrzy / 2))
                                                  (Point sceneWidth (wysokoscOstrzy / 2)))
                    { bodyCategoryBitMask = categoryBitMask [Swiat] }
                }

-- | Stwórz i przesuwaj przeszkody po scenie
przeszkody :: LambdaNode
przeszkody = (node [])
        { nodeActionDirectives = [odtwarzajListeAkcjiWNieskonczonosc 
                                  [ customAction (umiescParePrzeskod wysokoscBohatera)
                                  , waitForDuration{ actionDuration = 3 } --w sekundach
                                  ] ]
        , nodeUserData         = StanPrzeszkod generujLosowaLiczbe 
        }

-- | Pole z aktualnym wynikiem gracza
wynik :: LambdaNode
wynik = (labelNodeWithFontNamed "Verdana")
        { nodeName      = Just "Wynik"
        , nodePosition  = Point (sceneWidth / 2) (4 * sceneHeight / 5)
        , nodeZPosition = 100
        , nodeXScale = 3
        , nodeYScale = 3
        , labelText     = "0"
        }

-- | Aktualizuje scene na podstawie zdarzeń i aktualnego stanu gry
update :: LambdaScene -> TimeInterval -> LambdaScene
update scene@Scene{ sceneData = sceneState@StanSceny{..} } _dt 
  = case gameState of
      WTrakcieGry 
        | keyPressed -> przyspieszLambda scene{ sceneData = sceneState{ keyPressed = False } }
        | leftKeyPressed -> rocketTurn scene{ sceneData = sceneState{ leftKeyPressed = False } } True
        | rightKeyPressed -> rocketTurn scene{ sceneData = sceneState{ rightKeyPressed = False } } False
        | bumpScore  -> incScore scene{ sceneData = sceneState{ bumpScore = False } }
      Wypadek        -> crash scene{ sceneData = sceneState{ gameState = Koniec } }
      Koniec         -> scene

-- | Przyspiesza rakiete (podskok)
przyspieszLambda :: LambdaScene -> LambdaScene
przyspieszLambda scene
  = scene { sceneActionDirectives = [odtworzWlasnaAkcjeNa "Lambda" actionJump] }
  
-- | Trun the rocket
rocketTurn :: LambdaScene -> Bool -> LambdaScene
rocketTurn scene isLeftTurn
  = scene { sceneActionDirectives = [odtworzWlasnaAkcjeNa "Lambda" (actionTurn isLeftTurn)] }

-- | Zderzenie rakiety z obiektem fizycznym
crash :: LambdaScene -> LambdaScene
crash scene
  = scene { sceneActionDirectives = [ odtworzAkcjeNa "Lambda" crashAction
                                    , odtworzAkcjeNa "ObiektyWRuchu" stopMoving
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
    { sceneActionDirectives = [odtworzWlasnaAkcjeNa "Wynik" setScore]
    , sceneData             = sceneState{ sceneScore = newScore }
    }
  where
    newScore = sceneScore sceneState + 1

    setScore label@Label{} _dt = label{ labelText = show newScore }
    setScore node          _   = node

contact :: StanSceny 
        -> PhysicsContact u
        -> (Maybe StanSceny, Maybe (Node u), Maybe (Node u))
contact state@StanSceny{..} PhysicsContact{..}
  | (isWorld contactBodyA || isWorld contactBodyB) && gameState == WTrakcieGry
  = (Just state{ gameState = Wypadek }, Nothing, Nothing)
  | isScore contactBodyA || isScore contactBodyB
  = (Just state{ bumpScore = True }, Nothing, Nothing)
  | otherwise
  = (Nothing, Nothing, Nothing)  

-- | Obsługa eventów (naciskanie klawiszy)
handleEvent :: Event -> StanSceny -> Maybe StanSceny
handleEvent KeyEvent{ keyEventType = KeyDown } state = Just state{ keyPressed = True }
handleEvent MouseEvent{ mouseEventType = LeftMouseDown } state = Just state{ leftKeyPressed = True }
handleEvent MouseEvent{ mouseEventType = RightMouseDown } state = Just state{ rightKeyPressed = True }
handleEvent _                                  _     = Nothing
