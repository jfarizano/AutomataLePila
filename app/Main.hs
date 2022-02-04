{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Monad.Trans
import Control.Monad.Catch (MonadMask)
import Control.Exception ( catch , IOException )
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import System.Console.CmdArgs.Implicit
import System.FilePath ( splitExtension )
import System.IO ( hPrint, stderr, hPutStrLn )

import Lang
import Parse ( parsePDA, lexer )
import Global ( Env(..) )
import Monad
import Eval ( evalPDA )

opts = Env { 
    lastFile = def &= argPos 0 &= typ "FILE",
    checkOrder = FirstGiven &=help "Criterio de elección de transiciones" &= typ "first/random",
    graphic = False &=help "Graficar autómata",
    verbose = False &=help "Imprime en consola todas las transiciones realizadas"
  }
  &= summary "Autómata Le Pila, (C) Juan Ignacio Farizano"
  &= program "PDA"
  &= helpArg [help "Muestra esta lista de comandos"]
  &= versionArg [ignore]

main :: IO ()
main = cmdArgs opts >>= go
   where
       go :: Env -> IO ()
       go opts = do runPDA opts (runInputT defaultSettings repl)
                    return ()

prompt = "PDA> "

repl :: (MonadPDA m, MonadMask m) => InputT m () 
repl = do
    -- minput <- getInputLine prompt
    filename <- lift getLastFile
    let (_, ext) = splitExtension filename
    if ext /= ".pda"
    then do outputStrLn "Error: Nombre archivo inválido (nombre vacío o extensión incorrecta)"
            return ()
    else do pda <- lift $ loadFile filename
            outputStrLn $ show pda
            loop pda
            where
              -- loop :: Automaton -> InputT IO ()
              loop pda = do minput <- getInputLine prompt
                            case minput of
                               Nothing -> return ()
                               Just ":q" -> return ()
                               Just input -> do outputStrLn $ "Input was: " ++ input
                                                result <- lift $ evalPDA pda input (head $ states pda) ""
                                                outputStrLn $ show result
                                                loop pda

loadFile :: MonadPDA m => FilePath -> m Automaton
loadFile f = do
  x <- liftIO $ catch (readFile f)
              (\e -> do let err = show (e :: IOException)
                        hPutStrLn stderr ("No se pudo abrir el archivo " ++ f ++ ": " ++ err)
                        return "")
  setLastFile f
  return (parsePDA $ lexer x)