{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Monad where

import Lang
import Global
import Control.Monad.State
import Control.Monad.Except

class (MonadIO m, MonadState Env m, MonadError String m) => MonadPDA m where

setLastFile :: MonadPDA m => FilePath -> m ()
setLastFile f = modify (\s -> s {lastFile = f})

getLastFile :: MonadPDA m => m FilePath
getLastFile = gets lastFile

setVerbose :: MonadPDA m => Bool -> m ()
setVerbose b = modify (\s -> s {verbose = b})

getVerbose :: MonadPDA m => m Bool
getVerbose = gets verbose

setGraphic :: MonadPDA m => Bool -> m ()
setGraphic b = modify (\s -> s {graphic = b})

getGraphic :: MonadPDA m => m Bool
getGraphic = gets graphic

setActualPDA :: MonadPDA m => Automaton -> m ()
setActualPDA p = modify (\s -> s {actualPDA = p})

getActualPDA :: MonadPDA m => m Automaton
getActualPDA = gets actualPDA

printPDA :: MonadPDA m => String -> m ()
printPDA = liftIO . putStrLn

failPDA :: MonadPDA m => String -> m a
failPDA = throwError

catchPDA  :: MonadPDA m => m a -> m (Maybe a)
catchPDA c = catchError (Just <$> c) 
                        (\e -> liftIO $ putStrLn e >> return Nothing)

type PDA = StateT Env (ExceptT String IO)

instance MonadPDA PDA

runPDA :: Env -> PDA a -> IO (Either String (a, Env))
runPDA e m = runExceptT $ runStateT m e