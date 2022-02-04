module Eval ( evalPDA ) where

import Control.Monad ( foldM, mapM )
import Control.Monad.Trans

import Lang
import Global ( Env(..) )
import Monad
import PPrint

type Stack = [Symbol]
type Step = (String, State, Stack)

evalPDA :: MonadPDA m => Automaton -> String -> State -> Stack -> m Bool
evalPDA au w st sk | null w && st `elem` accStates au = do printVerbose $ "Palabra aceptada en el estado: " ++ st ++ ". Contenido restante de pila: " ++ sk
                                                           return True
                   | otherwise = do printVerbose $ "Estado actual: " ++ st ++ ". Palabra a leer: " ++ w ++ ". Contenido de la pila: " ++ sk
                                    steps <- nextSteps au w st sk
                                    case steps of
                                      [] -> do printVerbose $ "No hay pasos siguientes posibles a partir de acá."
                                               return False
                                      _ -> do printVerbose $ "Pasos siguientes posibles: " ++ show steps
                                              order <- getCheckOrder
                                              go order steps -- Para backtracking
                                              where go _ [] = do printVerbose $ "No quedaron más pasos, backtrackeando..."
                                                                 return False
                                                    go o s = do ((w', st', sk'), s') <- chooseStep o s
                                                                b <- evalPDA au w' st' sk'
                                                                if b then do return True
                                                                     else go o s'

nextSteps :: MonadPDA m => Automaton -> String -> State -> Stack -> m [Step]
nextSteps au w st sk = do transitions <- possibleTransitions au w st sk
                          printVerbose $ "Transiciones siguientes disponibles: " ++ show transitions
                          mapM (\t -> applyTransition w sk t) transitions

possibleTransitions :: MonadPDA m => Automaton -> String -> State -> Stack -> m [Transition]
possibleTransitions au w st sk = return $ filter go (transitions au)
                                 where go = (\(st', sy, hsk, _ , _)-> and [st == st',
                                                                           sy == 'λ' || (not (null w) && head w == sy),
                                                                           hsk == 'λ' || (not (null sk) && head sk == hsk)])

applyTransition :: MonadPDA m => String -> Stack -> Transition -> m Step
applyTransition w sk (_, sy, hsk, tsk, st) = return (w', st, sk')
                                             where
                                              w' = if sy == 'λ' then w else (if null w then w else tail w) -- Safe tail
                                              sk'' = if hsk == 'λ' then sk else tail sk
                                              sk' = if tsk == 'λ' then sk'' else tsk : sk''

chooseStep :: MonadPDA m => CheckOrder -> [Step] -> m (Step, [Step])
chooseStep FirstGiven (x : xs) = return (x, xs)
chooseStep Random xs = undefined