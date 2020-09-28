module Lib
  ( teletypeToIO,
    echo,
  )
where

import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Polysemy.Input ()
import Polysemy.Output ()

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Members '[Embed IO] r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

echo :: Members '[Teletype] r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _ -> writeTTY i >> echo
