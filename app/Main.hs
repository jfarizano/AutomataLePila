module Main where

import Control.Monad ( when, liftM )
import Control.Monad.Trans ( lift, liftIO )
import Control.Monad.Catch ( MonadMask )
import Control.Exception ( catch, IOException )

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
import Graphic

-- | Atributos y valores iniciales para el parser de argumentos. Contiene el 
-- estado global por defecto.
opts = Env {
    lastFile = def &= argPos 0 &= typFile &= opt "",
    verbose = False &= groupname "Opciones de ejecución" &= help "Imprime en consola todas las transiciones realizadas y demás datos relacionados a la ejecución del programa.",
    actualPDA = PDA { inputAlph = [], stackAlph = [], states = [], accStates = [], transitions = []} &= ignore,
    canRunPDA = False &= ignore,
    hSep = 1 &= name "hs" &= groupname "Opciones gráficas" &= help "Distancia horizontal entre nodos en el gráfico." &= typ "NUM",
    vSep = 0.5 &= name "vs" &= help "Distancia vertical entre nodos en el gráfico." &= typ "NUM",
    dpi = 800 &= help "Densidad de resolución del gráfico (no la resolución en sí)." &= typ "NUM",
    transparentBg = False &= name "tr" &= help "Si se activa se grafica el autómata con fondo transparente."
  }
  &= summary "Autómata Le Pila, (C) Juan Ignacio Farizano."
  &= program "PDA"
  &= helpArg [help "Muestra esta lista de comandos.", name "h"]
  &= details ["Símbolo lambda para tener a mano: λ"]
  &= versionArg [ignore]
  &= noAtExpand

-- | Función principal que inicia el sistema.
main :: IO ()
main = cmdArgs opts >>= go
   where
       go :: Env -> IO ()
       go opts = do runPDA opts (runInputT defaultSettings repl)
                    return ()

prompt = "\ESC[32m" ++ "PDA> " ++ "\ESC[0m"

-- | Read-Eval-Print Loop
-- Realiza el loop interactivo en consola, recibiendo comandos e interpretándolos.
repl :: (MonadPDA m, MonadMask m) => InputT m () 
repl = do
    filename <- lift getLastFile
    if null filename
    then do lift $ ppWarning "No se dió ningún archivo como argumento."
            lift $ ppWarning "Iniciando sin autómata cargado."
            loop
    else do contents <- lift $ catchPDA $ loadFile filename
            case contents of
              Nothing -> do lift $ ppWarning "Iniciando sin autómata cargado."            
                            loop
              Just pda -> loop
    where
      loop = do minput <- getInputLine prompt
                case minput of
                  Nothing -> return ()
                  Just input -> do c <- liftIO $ interpretCommand input
                                   b <- lift $ catchPDA $ handleCommand c
                                   maybe loop (`when` loop) b

-- | Estructura de comandos realizables en consola.
data Command = Eval String
             | Verbose
             | PPrintPDA
             | Graphic FilePath
             | Reload
             | Load FilePath
             | Help
             | Quit
             | Noop   -- No se dió un comando correcto y no se realiza nada.

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands 
  = [ Cmd [":verbose"] "" (const Verbose) "Togglea el nivel de verbose actual.",
      Cmd [":print"] "" (const PPrintPDA) "Imprime en consola el autómata actual.",
      Cmd [":graphic"] "" Graphic "Grafica el autómata y lo exporta al archivo dado.",
      Cmd [":reload"] "" (const Reload)  "Recarga el último archivo cargado.",
      Cmd [":load"] "<file>" Load "Carga un autómata desde un archivo.",
      Cmd [":help",":?"] "" (const Help) "Mostrar esta lista de comandos.",
      Cmd [":quit"] "" (const Quit) "Salir del programa."
    ]

-- | Recibe un string entrado por consola y devulve su representación
-- correspondiente como comando interactivo.
interpretCommand :: String -> IO Command
interpretCommand x = 
  if isPrefixOf ":" x 
  then do let (cmd, t') = break isSpace x
              (t, _) = span (not . isSpace) (dropWhile isSpace t')
              matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
          case matching of
            []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                        return Noop
            [Cmd _ _ f _] ->  do  return (f t)
            _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                  concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                        return Noop
  else return (Eval x)

-- | Interpreta un comando y devuelve un booleano indicando si se debe salir del
-- programa o no.
handleCommand ::  MonadPDA m => Command -> m Bool
handleCommand cmd = do
  b <- getCanRunPDA
  if not b
  then case cmd of
        Quit    -> return False
        Noop    -> return True
        Help    -> printPDA (helpTxt commands) >> return True
        Verbose -> toggleVerbose >> return True
        Reload  -> reloadFile >> return True
        Load f  -> loadFile f >> return True
        _       -> do ppError $ "No se encuentra un autómata válido cargado."
                      return True 
  else case cmd of
        Quit       -> return False
        Noop       -> return True
        Help       -> printPDA (helpTxt commands) >> return True
        Verbose    -> toggleVerbose >> return True
        PPrintPDA  -> getActualPDA >>= ppPDA >> return True
        Graphic f  -> graphicPDA f >> return True
        Reload     -> reloadFile >> return True
        Load f     -> loadFile f >> return True
        Eval w     -> checkWord w >> return True

-- | Función que devuelve el texto a imprimir por el comando de ayuda.
helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<word>                  verifica si el autómata cargado acepta la palabra.\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | Función usada por el comando verbose, si el modo verbose está desactivado
-- lo activa y viceversa.
toggleVerbose :: MonadPDA m => m ()
toggleVerbose = do b <- getVerbose
                   ppOpDone $ "Verbose fue " ++ if b then "desactivado." 
                                                     else "activado."
                   setVerbose $ not b

-- | Función que realiza el trabajo del comando de cargar archivo.
loadFile :: MonadPDA m => FilePath -> m ()
loadFile f = do ppOpDone $ "Abriendo archivo " ++ f
                setLastFile f
                x <- catchPDA $ readPDA f
                case x of
                  Nothing -> do setCanRunPDA False
                                ppWarning "No se pudo cargar el autómata, cargue uno válido antes de seguir."
                  Just pda -> do setActualPDA pda
                                 setCanRunPDA True
                                 ppVerbose "Autómata cargado."
                                 doIfVerbose ppPDA pda
                                 ppOpDone "El archivo fue cargado."

-- | Función que realiza el comando reload, hace un loadFile sobre el último
-- archivo cargado previamente.
reloadFile :: MonadPDA m => m ()
reloadFile = getLastFile >>= loadFile

-- | Dado un archivo carga en el estado global el autómata escrito en él si
-- este es un autómata válido.
readPDA :: MonadPDA m => FilePath -> m Automaton
readPDA f = do
  let (name, ext) = splitExtension f
  if null name || ext /= ".pda"
  then failPDA "Nombre archivo inválido (nombre vacío o extensión incorrecta)."
  else do x <- liftIO $ catch (Right `liftM` readFile f) (return . Left)
          case x of
            Left e -> failPDA $ "No se pudo leer el archivo. " ++ show (e :: IOException)
            Right contents -> case parsePDA contents of
                                Left e -> failPDA e
                                Right pda -> do b <- verifyPDA pda
                                                if b
                                                then return pda
                                                else failPDA "Se intentó cargar un autómata inválido."

-- | Dada una palabra chequea si el autómata cargado en el estado global
-- la reconoce.
checkWord :: MonadPDA m => String -> m Bool
checkWord w = do printPDA $ if null w then "La palabra vacía fue entrada."
                                      else "La palabra entrada fue: " ++ w ++ "."
                 pda <- getActualPDA
                 if and $ map (\c -> c `elem` (inputAlph pda)) w
                 then do result <- evalPDA pda w
                         ppResult result
                         return result
                 else do printPDA "La palabra contiene caracteres que no se encuentran en el alfabeto de entrada."
                         ppResult False
                         return False