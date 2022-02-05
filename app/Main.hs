{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad ( when )
import Control.Monad.Trans ( lift, liftIO )
import Control.Monad.Catch ( MonadMask )
import Control.Exception ( catch , IOException )
import System.Console.Haskeline ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import System.Console.CmdArgs.Implicit
import System.FilePath ( splitExtension )
import System.IO ( hPrint, stderr, hPutStrLn )

import Data.List ( intersperse, isPrefixOf )
import Data.Char ( isSpace )

import Lang
import Parse
import Global
import Monad
import Eval
import PPrint
import Lib

opts = Env {
    lastFile = def &= argPos 0 &= typFile &= opt "",
    graphic = False &=help "Graficar autómata.",
    verbose = False &=help "Imprime en consola todas las transiciones realizadas.",
    actualPDA = PDA { inputAlph = [], stackAlph = [], states = [], accStates = [], transitions = []} &= ignore
  }
  &= summary "Autómata Le Pila, (C) Juan Ignacio Farizano."
  &= program "PDA"
  &= helpArg [help "Muestra esta lista de comandos."]
  &= details ["Símbolo lambda para tener a mano: λ"]
  &= versionArg [ignore]

main :: IO ()
main = cmdArgs opts >>= go
   where
       go :: Env -> IO ()
       go opts = do if null $ lastFile opts
                      then do runPDA opts (runInputT defaultSettings waitForFile)
                              return ()
                      else do runPDA opts (runInputT defaultSettings repl)
                              return ()

prompt = "PDA> "

repl :: (MonadPDA m, MonadMask m) => InputT m () 
repl = do
    filename <- lift getLastFile
    let (name, ext) = splitExtension filename
    if ext /= ".pda"
    then do outputStrLn "Error: Nombre archivo inválido (nombre vacío o extensión incorrecta)."
            waitForFile
    else do contents <- lift $ catchPDA $ loadFile filename
            case contents of
                Nothing -> do outputStrLn "Intente cargar otro archivo o arreglar el actual."
                              waitForFile
                Just pda -> do lift $ printVerbose $ show pda
                               lift $ setActualPDA pda
                               loop
                            where
                              loop = do minput <- getInputLine prompt
                                        case minput of
                                           Nothing -> return ()
                                           Just input -> do c <- liftIO $ interpretCommand input
                                                            b <- lift $ catchPDA $ handleCommand c
                                                            maybe loop (`when` loop) b

waitForFile :: (MonadPDA m, MonadMask m) => InputT m () 
waitForFile = return ()

data Command = Eval String
             | Verbose
             | PPrintPDA
             | Reload
             | Load FilePath
             | Help
             | Quit
             | Noop   -- No se puso un comando correcto y no se realiza nada

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands 
  = [ Cmd [":verbose"] "" (const Verbose) "Togglea el nivel de verbose actual.",
      Cmd [":print"] "" (const PPrintPDA) "Imprime en consola el autómata actual.",
      Cmd [":reload"] "" (const Reload)  "Recarga el último archivo cargado.",
      Cmd [":load"] "<file>" Load "Carga un autómata desde un archivo.",
      Cmd [":help",":?"] "" (const Help) "Mostrar esta lista de comandos.",
      Cmd [":quit"] "" (const Quit) "Salir del programa."
    ]

interpretCommand :: String -> IO Command
interpretCommand x = if isPrefixOf ":" x 
                     then do let (cmd, t') = break isSpace x
                                 t = dropWhile isSpace t'
                                 matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
                             case matching of
                               []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                                           return Noop
                               [Cmd _ _ f _] ->  do  return (f t)
                               _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                                     concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                                           return Noop
                     else return (Eval x)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPDA m => Command -> m Bool
handleCommand cmd = do
  case cmd of
      Quit          -> return False
      Noop          -> return True
      Help          -> printPDA (helpTxt commands) >> return True
      Verbose       -> do b <- getVerbose
                          printPDA $ "Verbose fue " ++ if b then "desactivado." else "activado."
                          setVerbose $ not b
                          return True
      PPrintPDA     -> getActualPDA >>= ppPrintPDA >> return True
      Reload        -> (getLastFile >>= (\f -> loadFile f) >>= setActualPDA) >> printPDA "El archivo fue recargado." >> return True
      Load f        -> (printPDA $ "Abriendo archivo: " ++ f) >> (loadFile f >>= setActualPDA) >> return True
      Eval w        -> do printPDA $ if null w then "La palabra vacía fue entrada." else "La palabra entrada fue: " ++ w
                          pda <- getActualPDA
                          result <- evalPDA pda w
                          printPDA $ (if result then "La palabra fue aceptada." 
                                                else "La palabra no fue aceptada.")
                          return True

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<word>                  verifica si el autómata cargado acepta la palabra\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

loadFile :: MonadPDA m => FilePath -> m Automaton
loadFile f = do
  x <- liftIO $ catch (readFile f)
              (\e -> do let err = show (e :: IOException)
                        hPutStrLn stderr ("No se pudo abrir el archivo " ++ f ++ ": " ++ err)
                        return "")
  if null x
    then failPDA "Archivo vacío/inexistente"
    else case readPDA x of
          Left e -> failPDA e
          Right pda -> verifyPDA pda