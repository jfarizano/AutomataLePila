module Lib where

import Lang
import Monad
import Global

verifyPDA :: MonadPDA m => Automaton -> m Bool
verifyPDA au = do balph <- verifyAlph ialph salph
                  baccs <- verifyAccStates st accSt
                  bt <- verifyTransitions tr st ialph salph
                  return $ and [balph, baccs, bt]
               where 
                 ialph = inputAlph au
                 salph = stackAlph au
                 st = states au
                 accSt = accStates au
                 tr = transitions au

verifyAlph :: MonadPDA m => Alphabet -> Alphabet -> m Bool
verifyAlph ialph salph = do bl <- mapM go ialph
                            return $ and bl
                         where
                           go c = if c `elem` salph
                                    then return True
                                    else do printPDA $ "El símbolo " ++ show c ++ " del alfabeto de entrada no pertenece al alfabeto de pila."
                                            return False

verifyAccStates :: MonadPDA m => [State] -> [State] -> m Bool
verifyAccStates st accSt = do bl <- mapM go accSt
                              return $ and bl
                           where
                             go s = if s `elem` st
                                    then return True
                                    else do printPDA $ "El estado de aceptación " ++ show s ++ " no está dado como un estado."
                                            return False

verifyTransitions :: MonadPDA m => [Transition] -> [State] -> Alphabet -> Alphabet -> m Bool
verifyTransitions tr st ialph salph = do bl <- mapM go tr
                                         return $ and bl
                                      where
                                        go t@(st0, sy0, sy1, sy2, st1) = do b0 <- stt t st0
                                                                            b1 <- stt t st1
                                                                            b2 <- symIn t sy0
                                                                            b3 <- symSk t sy1
                                                                            b4 <- symSk t sy2
                                                                            return $ and [b0, b1, b2, b3, b4]
                                        symIn t c = if c == 'λ' || c `elem` ialph
                                                    then return True
                                                    else do printPDA $ "En la transición " ++ show t ++ " el caracter " ++ show c ++ " no pertenece al alfabeto de entrada."
                                                            return False
                                        symSk t c = if c == 'λ' || c `elem` salph
                                                    then return True
                                                    else do printPDA $ "En la transición " ++ show t ++ " el caracter " ++ show c ++ " no pertenece al alfabeto de pila."
                                                            return False
                                        stt t s = if s `elem` st
                                                  then return True
                                                  else do printPDA $ "En la transición " ++ show t ++ " el estado " ++ show s ++ " no es un estado válido."
                                                          return False