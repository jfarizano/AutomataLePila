module Eval ( evalPDA ) where

import Control.Monad ( foldM, mapM )
import Control.Monad.Trans

import Lang
import Global ( Env(..) )
import Monad
import PPrint


evalPDA :: MonadPDA m => Automaton -> State -> String -> Stack -> m Bool
evalPDA au st w sk | null w && st `elem` accStates au = do printVerbose $ "Palabra aceptada en el estado: " ++ st ++ ". Contenido restante de pila: " ++ sk
                                                           return True
                   | otherwise = do printVerbose $ "Estado actual: " ++ st ++ ". Palabra a leer: " ++ w ++ ". Contenido de la pila: " ++ sk
                                    steps <- nextSteps au st w sk
                                    case steps of
                                      [] -> do printVerbose $ "No hay pasos siguientes posibles a partir de acá."
                                               return False
                                      _ -> do printVerbose $ "Pasos siguientes posibles: " ++ show steps
                                              go steps -- Para backtracking
                                              where go [] = do printVerbose $ "No quedaron más pasos, backtrackeando..."
                                                               return False
                                                    go ((st', w', sk') : s') = do b <- evalPDA au st' w' sk'
                                                                                  if b then do return True
                                                                                       else go s'

nextSteps :: MonadPDA m => Automaton -> State -> String -> Stack -> m [Step]
nextSteps au st w sk = do transitions <- possibleTransitions au st w sk
                          printVerbose $ "Transiciones siguientes disponibles: " ++ show transitions
                          mapM (\t -> applyTransition w sk t) transitions

possibleTransitions :: MonadPDA m => Automaton -> State -> String -> Stack -> m [Transition]
possibleTransitions au st w sk = return $ filter go (transitions au)
                                 where go = (\(st', sy, hsk, _, _)-> and [st == st',
                                                                          sy == 'λ' || (not (null w) && head w == sy),
                                                                          hsk == 'λ' || (not (null sk) && head sk == hsk)])

applyTransition :: MonadPDA m => String -> Stack -> Transition -> m Step
applyTransition w sk (_, sy, hsk, tsk, st) = return (st, w', sk')
                                             where
                                              w' = if sy == 'λ' then w else (if null w then w else tail w) -- Safe tail
                                              sk'' = if hsk == 'λ' then sk else tail sk
                                              sk' = if tsk == 'λ' then sk'' else tsk : sk''