module LosoweLiczby where

import System.Random
import System.IO.Unsafe

-- | Generuje losową liczbę
generujLosowaLiczbe :: (Random a, Num a) => [a]
generujLosowaLiczbe = unsafePerformIO $ randoms <$> newStdGen
