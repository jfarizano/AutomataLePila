module PPrint where

import Lang
import Monad

printVerbose :: MonadPDA m => String -> m ()
printVerbose s = do { v <- getVerbose ; if v then printPDA s else return () }

ppPrintPDA :: MonadPDA m => Automaton -> m ()
ppPrintPDA = undefined