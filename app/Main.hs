module Main where

import Lib ( teletypeToIO, echo )
import Polysemy (runM)

main :: IO ()
main = runM . teletypeToIO $ echo