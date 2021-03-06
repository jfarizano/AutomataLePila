module Lib ( verifyPDA ) where

import Lang
import Monad
import Global
import PPrint

-- | Dado un autómata verifica si este es válido.
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

-- | Dados los alfabetos de entrada y pila verifica que todos los símbolos
-- del alfabeto de entrada también se encuentren en el de pila.
verifyAlph :: MonadPDA m => Alphabet -> Alphabet -> m Bool
verifyAlph ialph salph = do bl <- mapM go ialph
                            return $ and bl
                         where
                           go c = if c `elem` salph
                                  then return True
                                  else do ppErrorStackSy c
                                          return False

-- | Dadas las listas de estados y estados de aceptación verifica que todos
-- los estados de aceptación también sean estados dados.
verifyAccStates :: MonadPDA m => [State] -> [State] -> m Bool
verifyAccStates st accSt = do bl <- mapM go accSt
                              return $ and bl
                           where
                             go s = if s `elem` st
                                    then return True
                                    else do ppErrorAccState s
                                            return False

-- | Verifica que en las transiciones todas las componentes de las tuplas sean
-- válidas.
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
                                                    else do ppErrorTrInSy t c
                                                            return False
                                        symSk t c = if c == 'λ' || c `elem` salph
                                                    then return True
                                                    else do ppErrorTrSkSy t c
                                                            return False
                                        stt t s = if s `elem` st
                                                  then return True
                                                  else do ppErrorTrState t s
                                                          return False