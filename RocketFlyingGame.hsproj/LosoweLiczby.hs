module LosoweLiczby where
  
import System.Random
import System.IO.Unsafe


generujLosowaLiczbe :: (Random a, Num a) => [a]
generujLosowaLiczbe = unsafePerformIO $ randoms <$> newStdGen
