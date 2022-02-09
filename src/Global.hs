{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import System.Console.CmdArgs.Implicit ( Data, Typeable )

import Lang

-- | Estado global del sistema, contiene datos relevantes al loop interactivo 
-- en consola y al módulo de graficar autómatas.
data Env = Env {
    -- |  Datos interactivos
    lastFile :: FilePath, -- Último archivo cargado o que se intentó cargar (para permitir reload en caso de error).
    verbose :: Bool, -- Imprimir o no en consola información extra sobre cada paso realizado en la ejecución.
    actualPDA :: Automaton, -- Último autómata cargado en memoria.
    canRunPDA :: Bool, -- El último autómata cargado es válido y se pueden ejecutar operaciones relacionadas a él.
    -- | Datos sobre gráfico
    hSep :: Double, -- Distancia horizontal entre nodos en el gráfico.
    vSep :: Double, -- Distancia vertical entre nodos en el gráfico.
    dpi :: Double, -- DPI de la imagen resultante.
    transparentBg :: Bool -- Graficar o no con fondo transparente.
} deriving (Show, Data, Typeable)