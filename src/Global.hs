{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import System.Console.CmdArgs.Implicit ( Data, Typeable )

import Lang

data Env = Env {
    lastFile :: FilePath,
    graphic :: Bool,
    verbose :: Bool,
    actualPDA :: Automaton
} deriving (Show, Data, Typeable)
