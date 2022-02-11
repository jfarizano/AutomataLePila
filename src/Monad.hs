{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Monad where

import Lang
import Global
import Control.Monad.State
import Control.Monad.Except

-- | Defino la mónada MonadPDA que me permite realizar operaciones de
-- input/output, llevar un estado de tipo Env y manejo de errores.
class (MonadIO m, MonadState Env m, MonadError String m) => MonadPDA m where

setLastFile :: MonadPDA m => FilePath -> m ()
setLastFile f = modify (\s -> s {lastFile = f})

getLastFile :: MonadPDA m => m FilePath
getLastFile = gets lastFile

setVerbose :: MonadPDA m => Bool -> m ()
setVerbose b = modify (\s -> s {verbose = b})

getVerbose :: MonadPDA m => m Bool
getVerbose = gets verbose

setActualPDA :: MonadPDA m => Automaton -> m ()
setActualPDA p = modify (\s -> s {actualPDA = p})

getActualPDA :: MonadPDA m => m Automaton
getActualPDA = gets actualPDA

setCanRunPDA :: MonadPDA m => Bool -> m ()
setCanRunPDA b = modify (\s -> s {canRunPDA = b})

getCanRunPDA :: MonadPDA m => m Bool
getCanRunPDA = gets canRunPDA

gethSep :: MonadPDA m => m Double
gethSep = gets hSep

sethSep :: MonadPDA m => Double -> m ()
sethSep h = modify (\s -> s {hSep = h})

getvSep :: MonadPDA m => m Double
getvSep = gets vSep

setvSep :: MonadPDA m => Double -> m ()
setvSep v = modify (\s -> s {vSep = v})

getDpi :: MonadPDA m => m Double
getDpi = gets dpi

setDpi :: MonadPDA m => Double -> m ()
setDpi d = modify (\s -> s {dpi = d})

getTransparentBg :: MonadPDA m => m Bool
getTransparentBg = gets transparentBg

setTransparentBg :: MonadPDA m => Bool -> m ()
setTransparentBg b = modify (\s -> s {transparentBg = b})

printPDA :: MonadPDA m => String -> m ()
printPDA = liftIO . putStrLn

failPDA :: MonadPDA m => String -> m a
failPDA = throwError

catchPDA :: MonadPDA m => m a -> m (Maybe a)
catchPDA c = catchError (Just <$> c) 
                        (\e -> liftIO $ putStrLn ("\ESC[31m" ++ "[ERROR] " ++ "\ESC[0m" ++ e) >> return Nothing)

-- Función utilizada principalmente para realizar una computación solamente
-- cuando se encuentra el modo verbose activado, por ej una función de pretty
-- printing con un argumento dado.
doIfVerbose :: MonadPDA m => (a -> m ()) -> a -> m ()
doIfVerbose f a = do v <- getVerbose
                     if v then f a
                          else return ()

type PDA = StateT Env (ExceptT String IO)

-- | Esta es una instancia vacía, ya que 'MonadPDA' no tiene funciones miembro.
instance MonadPDA PDA

-- | Corre una computación de MonadPDA con el estado dado.
runPDA :: Env -> PDA a -> IO (Either String (a, Env))
runPDA e m = runExceptT $ runStateT m e