module Eval where

import Lang
import Global ( Env(..) )
import Monad
import Control.Monad ( foldM, mapM )
import Control.Monad.Trans

type Stack = [Symbol]
type Step = (String, State, Stack)

evalPDA :: MonadPDA m => Automaton -> String -> State -> Stack -> m Bool
evalPDA au w st sk | null w && st `elem` accStates au = return True
                   | otherwise = do steps <- nextSteps au w st sk
                                    case steps of
                                      [] -> return False
                                      _ -> do order <- getCheckOrder
                                              go order steps -- Para backtracking
                                              where go _ [] = return False
                                                    go o _ = do ((w', st', sk'), steps') <- chooseStep o steps
                                                                b <- evalPDA au w' st' sk'
                                                                if b then return True else go o steps'

nextSteps :: MonadPDA m => Automaton -> String -> State -> Stack -> m [Step]
nextSteps au w st sk = do transitions <- possibleTransitions au w st sk
                          mapM (\t -> applyTransition w sk t) transitions                  

possibleTransitions :: MonadPDA m => Automaton -> String -> State -> Stack -> m [Transition]
possibleTransitions au w st sk = return $ filter go (transitions au)
                                 where go = (\(st', hsk, sy, _, _)-> and [st == st', 
                                                                          hsk == 'λ' || (not (null sk) && head sk == hsk), 
                                                                          sy == 'λ' || (not (null w) && head w == sy)])

applyTransition :: MonadPDA m => String -> Stack -> Transition -> m Step
applyTransition w sk (_, hsk, sy, tsk, st) = return (w', st, sk')
                                             where 
                                              w' = if sy == 'λ' then w else (if null w then w else tail w) -- Safe tail
                                              sk'' = if hsk == 'λ' then sk else (if null sk then sk else tail sk)
                                              sk' = if tsk == 'λ' then sk'' else tsk : sk''

chooseStep :: MonadPDA m => CheckOrder -> [Step] -> m (Step, [Step])
chooseStep FirstGiven xs = return (head xs, if null xs then [] else tail xs)
chooseStep Random xs = undefined