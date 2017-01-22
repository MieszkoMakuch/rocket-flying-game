{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module FunkcjePomocnicze where
  
import Graphics.SpriteKit


-- Stwórz teksturę na podstawie obrazka w podanym pliku, oraz określ
-- jej szerokość i wysokość na podstawie wymiarow obrazka
stworzTeksture :: FilePath -> (Texture, GFloat, GFloat)
stworzTeksture path = (tex, sizeWidth, sizeHeight)
  where
    tex      = textureWithImageNamed path
    Size{..} = textureSize tex

-- Odtwarza podaną akcję na podanym węźle (child node)
odtworzAkcjeNa :: String -> Action children -> SDirective node children
odtworzAkcjeNa childName action 
  = runAction $ runActionOnChildWithName action childName

-- Odtwarza podaną akcję (zdefiniowaną w Akcje.hs) na podanym węźle (child node).
-- Wykorzystuje funkcje odtworzAkcjeNa
odtworzWlasnaAkcjeNa :: String -> TimedUpdate children -> SDirective node children
odtworzWlasnaAkcjeNa childName actionFun 
  = odtworzAkcjeNa childName $ customAction actionFun

-- Odtwarza podaną akcję w nieskończoność
odtwarzajAkcjeWNieskonczonosc :: SAction node children -> SDirective node children
odtwarzajAkcjeWNieskonczonosc = runAction . repeatActionForever

-- Odtwarza podana liste akcji w nieskonczoność
-- Wykorzystuje funkcje odtwarzajAkcjeWNieskonczonosc
odtwarzajListeAkcjiWNieskonczonosc :: [SAction n c] -> SDirective n c
odtwarzajListeAkcjiWNieskonczonosc = odtwarzajAkcjeWNieskonczonosc . sequenceActions