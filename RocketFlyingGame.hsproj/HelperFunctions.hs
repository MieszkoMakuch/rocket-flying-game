{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module HelperFunctions where
  
import Graphics.SpriteKit
import System.IO.Unsafe
import System.Random


-- | Create texture based on the provided image path and set its width 
-- | and height based on the image size
createTexture :: FilePath -> (Texture, GFloat, GFloat)
createTexture path = (tex, sizeWidth, sizeHeight)
  where
    tex      = textureWithImageNamed path
    Size{..} = textureSize tex

-- | Plays a specified action on specified node (child node).
playActionOn :: String -> Action children -> SDirective node children
playActionOn childName action 
  = runAction $ runActionOnChildWithName action childName

-- | Plays a custom action (defined in Actions.hs) on the specified node (child node). 
-- | Uses playAcionOn function.
playCustomActionOn :: String -> TimedUpdate children -> SDirective node children
playCustomActionOn childName actionFun 
  = playActionOn childName $ customAction actionFun

-- | Plays a specified action in the loop.
playAcionInLoop :: SAction node children -> SDirective node children
playAcionInLoop = runAction . repeatActionForever

-- | Plays specified sequence (list) of actions in the loop. 
-- | Uses playAcionInLoop function
playSequenceOfActionsInLoop :: [SAction n c] -> SDirective n c
playSequenceOfActionsInLoop = playAcionInLoop . sequenceActions

-- | Generates a random number
getRandomNumber :: (Random a, Num a) => [a]
getRandomNumber = unsafePerformIO $ randoms <$> newStdGen