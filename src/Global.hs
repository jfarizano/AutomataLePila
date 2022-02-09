{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import System.Console.CmdArgs.Implicit ( Data, Typeable )

import Lang

data Env = Env {
    -- Datos interactivos
    lastFile :: FilePath,
    verbose :: Bool,
    actualPDA :: Automaton,
    canRunPDA :: Bool,
    -- Datos sobre gráfico
    hSep :: Double,
    vSep :: Double,
    dpi :: Double,
    transparentBg :: Bool
} deriving (Show, Data, Typeable)