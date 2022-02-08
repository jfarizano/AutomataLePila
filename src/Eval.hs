module Eval ( evalPDA ) where

import Lang
import Global
import Monad
import PPrint

evalPDA :: MonadPDA m => Automaton -> String -> m Bool
evalPDA au w = evalPDA' au (head $ states au) w ""

evalPDA' :: MonadPDA m => Automaton -> State -> String -> Stack -> m Bool
evalPDA' au st w sk | null w && st `elem` accStates au = do ppEvalVerboseAcc st sk
                                                            return True
                    | otherwise = do ppEvalVerboseActual st w sk
                                     steps <- nextSteps au st w sk
                                     case steps of
                                       [] -> do ppVerbose "No hay pasos siguientes posibles a partir de acá."
                                                return False
                                       _ -> do ppVerbose "Pasos siguientes posibles: "
                                               doIfVerbose ppSteps steps
                                               go steps -- Para backtracking
                                               where go [] = do ppVerbose "No quedaron más pasos, backtrackeando..."
                                                                return False
                                                     go ((st', w', sk') : s') = do b <- evalPDA' au st' w' sk'
                                                                                   if b then do return True
                                                                                        else do ppEvalVerboseBacktr st w sk
                                                                                                go s'

nextSteps :: MonadPDA m => Automaton -> State -> String -> Stack -> m [Step]
nextSteps au st w sk = do transitions <- possibleTransitions au st w sk
                          ppVerbose "Transiciones siguientes disponibles: "
                          doIfVerbose ppTransitions transitions 
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