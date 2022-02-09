module Eval ( evalPDA ) where

import Lang
import Global
import Monad
import PPrint

-- | Función de evaluación del autómata, dado un autómata y una palabra, devuelve
-- un booleano de acuerdo a si el autómata reconoce esa palabra
-- Utiliza la función auxiliar evalPDA' pasandole la configuración inicial de la máquina.
evalPDA :: MonadPDA m => Automaton -> String -> m Bool
evalPDA au w = evalPDA' au (head $ states au) w ""

-- | Función auxiliar que evalúa al autómata, recibe la configuración de la máquina
-- y la palabra a reconocer. Si se recibe la palabra vacía y se está en un estado
-- de aceptación entonces la palabra es reconocida y se devuelve True, en otro caso
-- se obtienen las próximas configuraciones posibles a partir de la actual
-- y evalúa recursivamente, si no hay más pasos disponibles la palabra no es 
-- reconocida y se devuelve False, de esta forma realizamos backtracking si la 
-- llamada actual fue recursiva.
evalPDA' :: MonadPDA m => Automaton -> State -> String -> Stack -> m Bool
evalPDA' au st w sk | null w && st `elem` accStates au = do ppEvalVerboseAcc st sk
                                                            return True
                    | otherwise = do ppEvalVerboseActual st w sk
                                     configs <- nextConfigs au st w sk
                                     case configs of
                                       [] -> do ppVerbose "No hay pasos siguientes posibles a partir de acá."
                                                return False
                                       _ -> do ppVerbose "Pasos siguientes posibles: "
                                               doIfVerbose ppConfigs configs
                                               go configs -- Para backtracking
                                               where go [] = do ppVerbose "No quedaron más pasos, backtrackeando..."
                                                                return False
                                                     go ((st', w', sk') : s') = do b <- evalPDA' au st' w' sk'
                                                                                   if b then do return True
                                                                                        else do ppEvalVerboseBacktr st w sk
                                                                                                go s'

-- | Recibe un autómata y su configuración, a partir de esto devuelve todas las
-- configuraciones posibles por las que se puede seguir.
nextConfigs :: MonadPDA m => Automaton -> State -> String -> Stack -> m [Config]
nextConfigs au st w sk = do transitions <- possibleTransitions au st w sk
                            ppVerbose "Transiciones siguientes disponibles: "
                            doIfVerbose ppTransitions transitions 
                            mapM (\t -> applyTransition w sk t) transitions

-- | Recibe un autómata y su configuración, devuelve todas las transiciones aplicables
-- a la configuración actual.
possibleTransitions :: MonadPDA m => Automaton -> State -> String -> Stack -> m [Transition]
possibleTransitions au st w sk = return $ filter go (transitions au)
                                 where go = (\(st', sy, hsk, _, _)-> and [st == st',
                                                                          sy == 'λ' || (not (null w) && head w == sy),
                                                                          hsk == 'λ' || (not (null sk) && head sk == hsk)])

-- | Dado un autómata, una configuración y una transición aplica la transición
-- a la configuración dada y devuelve la nueva configuración.
applyTransition :: MonadPDA m => String -> Stack -> Transition -> m Config
applyTransition w sk (_, sy, hsk, tsk, st) = return (st, w', sk')
                                             where
                                              w' = if sy == 'λ' then w else (if null w then w else tail w) -- Safe tail
                                              sk'' = if hsk == 'λ' then sk else tail sk
                                              sk' = if tsk == 'λ' then sk'' else tsk : sk''