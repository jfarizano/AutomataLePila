{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import System.Console.CmdArgs.Implicit ( Data, Typeable )

import Lang

data Env = Env {
    lastFile :: FilePath,
    checkOrder :: CheckOrder,
    graphic :: Bool,
    verbose :: Bool
} deriving (Show, Data, Typeable)

initialEnv :: Env
initialEnv = Env "" FirstGiven False False