{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Import
import Options.Applicative.Simple
import qualified Paths_taskell2
import RIO.Process
import Run

main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            $(simpleVersion Paths_taskell2.version)
            "taskell"
            "A command-line Kanban board"
            (Options <$> switch (long "verbose" <> short 'v' <> help "Verbose output?"))
            empty
    lo <- logOptionsHandle stderr (optionsVerbose options)
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
        let app = App {appLogFunc = lf, appProcessContext = pc, appOptions = options}
         in runRIO app run
