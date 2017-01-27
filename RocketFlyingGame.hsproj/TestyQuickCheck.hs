module TestyQuickCheck where

import Prelude hiding (reverse)
import Test.QuickCheck


-- | Nie znalazlem zadnego zastosowania dla QuickCheck w moim projekcie,
-- | dlatego napisalem przykladowy test dla funkcji odwracajacej liste

-- | Klasyczna, wolniejsza wersja odwracania list
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- | Odwrocona 2 krotnie lista jest ta sama lista
prop_revRev :: [String] -> Bool
prop_revRev xs =
  reverse (reverse xs) == xs

-- | Relacja pomiedzy odwracaniem listy a ich laczeniem
prop_revApp :: [Int] -> [Int] -> Bool
prop_revApp xs ys =
  reverse (xs ++ ys) == reverse ys ++ reverse xs

-- | Odwracanie listy w czasie liniowym
fastReverse :: [a] -> [a]
fastReverse xs = rev [] xs
  where
    rev rxs []     = rxs
    rev rxs (x:xs) = rev (x:rxs) xs

-- | Obydwa algorytmy powinny dac ten sam wynik 
prop_fastRev :: [Int] -> Bool
prop_fastRev xs =
  reverse xs == fastReverse xs


main = print $ fastReverse [1..100]