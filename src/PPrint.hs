module PPrint where

import Data.List ( intersperse, isPrefixOf )
import Data.Text ( unpack )
import Prettyprinter
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )

import Lang
import Monad
import Global

warningColor = annotate (color Yellow)
errorColor = annotate (color Red)
stateColor = annotate (color Green)
inputSymbolColor = annotate (color Blue)
inputColor = inputSymbolColor
stackSymbolColor = annotate (color Yellow)
stackColor = stackSymbolColor
verboseColor = annotate (color Magenta)
opDoneColor = annotate (color Green)

ppOpDone :: MonadPDA m => String -> m ()
ppOpDone = prettyPrint . opDone2doc

opDone2doc :: String -> Doc AnsiStyle
opDone2doc s = hsep [opDoneColor $ pretty "→", pretty s]

ppWarning :: MonadPDA m => String -> m ()
ppWarning = prettyPrint . warning2doc

warning2doc :: String -> Doc AnsiStyle
warning2doc w = hsep [warningColor $ pretty "[ADVERTENCIA]", pretty w]

ppError :: MonadPDA m => String -> m ()
ppError = prettyPrint . error2doc

error2doc :: String -> Doc AnsiStyle
error2doc e = hsep [errorColor $ pretty "[ERROR]", pretty e]

verbose2doc :: String -> Doc AnsiStyle
verbose2doc s = hsep [verboseColor $ pretty "[v]", pretty s]

ppVerbose :: MonadPDA m => String -> m ()
ppVerbose s = do v <- getVerbose
                 if v then printPDA $ render $ verbose2doc s
                      else return ()

ppPDA :: MonadPDA m => Automaton -> m ()
ppPDA au = prettyPrint $ vsep [opDone2doc "Imprimiendo autómata", pda2doc au]

pda2doc :: Automaton -> Doc AnsiStyle
pda2doc au = align $ vsep [pretty "Alfabeto de entrada: ",
                           inputAlph2doc $ inputAlph au,
                           pretty "Alfabeto de pila: ",
                           stackAlph2doc $ stackAlph au,
                           pretty "Estados: ",
                           states2doc $ states au,
                           pretty "Estados de aceptación: ",
                           states2doc $ accStates au,
                           pretty "Transiciones: ",
                           transitions2doc $ transitions au]

inputAlph2doc :: Alphabet -> Doc AnsiStyle
inputAlph2doc a = brackedList (map inputSymbol2doc a)

stackAlph2doc :: Alphabet -> Doc AnsiStyle
stackAlph2doc a = brackedList (map stackSymbol2doc a)

states2doc :: [State] -> Doc AnsiStyle
states2doc s = brackedList (map (stateColor . pretty) s)

ppTransitions :: MonadPDA m => [Transition] -> m ()
ppTransitions = prettyPrint . transitions2doc

transition2doc :: Transition -> Doc AnsiStyle
transition2doc (st0, sy0, sy1, sy2, st1) = tupled $ [state2doc st0,
                                                     inputSymbol2doc sy0,
                                                     stackSymbol2doc sy1,
                                                     stackSymbol2doc sy2,
                                                     state2doc st1]

transitions2doc :: [Transition] -> Doc AnsiStyle
transitions2doc t = brackedList (map transition2doc t)

word2doc :: String -> Doc AnsiStyle
word2doc = pretty

inputSymbol2doc :: Symbol -> Doc AnsiStyle
inputSymbol2doc = inputSymbolColor . pretty

stackSymbol2doc :: Symbol -> Doc AnsiStyle
stackSymbol2doc = stackSymbolColor . pretty

state2doc :: State -> Doc AnsiStyle
state2doc = stateColor . pretty

ppSteps :: MonadPDA m => [Step] -> m ()
ppSteps = prettyPrint . steps2doc

step2doc :: Step -> Doc AnsiStyle
step2doc (st, w, sk) = tupled $ [state2doc st, 
                                 if null w then word2doc "λ" else word2doc w,
                                 if null w then stack2doc "λ" else stack2doc sk]

steps2doc :: [Step] -> Doc AnsiStyle
steps2doc s = brackedList $ (map step2doc s)

stack2doc :: Stack -> Doc AnsiStyle
stack2doc = stackColor . pretty

brackedList :: [Doc AnsiStyle] -> Doc AnsiStyle
brackedList xs = encloseSep lbrace rbrace (pretty ", ") xs

ppEvalVerboseAcc :: MonadPDA m => State -> Stack -> m ()
ppEvalVerboseAcc st sk = doIfVerbose prettyPrint (cat [verbose2doc "Palabra aceptada en el estado: ",
                                                       state2doc st,
                                                       go sk])
                         where go "" = pretty ". La pila terminó vacía."
                               go sk = cat [pretty ". Contenido de la pila: ", stack2doc sk, dot]

ppEvalVerboseActual :: MonadPDA m => State -> String -> Stack -> m ()
ppEvalVerboseActual st w sk = doIfVerbose prettyPrint (cat [verbose2doc "Estado actual: ",
                                                            state2doc st,
                                                            verboseWord2doc w,
                                                            verboseStack2doc sk])

ppEvalVerboseBacktr :: MonadPDA m => State -> String -> Stack -> m ()
ppEvalVerboseBacktr st w sk = doIfVerbose prettyPrint (cat [verbose2doc "Se volvió al estado ",
                                                            state2doc st,
                                                            verboseWord2doc w,
                                                            verboseStack2doc sk])

verboseWord2doc :: String -> Doc AnsiStyle
verboseWord2doc "" = pretty ". No queda palabra para leer"
verboseWord2doc w = cat [pretty ". Palabra a leer: ", word2doc w]

verboseStack2doc :: Stack -> Doc AnsiStyle
verboseStack2doc "" = pretty ". La pila está vacía."
verboseStack2doc sk = cat [pretty ". Contenido de la pila: ", stack2doc sk, dot]

ppResult :: MonadPDA m => Bool -> m ()
ppResult True = prettyPrint $ sep [pretty "La palabra fue", cat [annotate (color Green) $ pretty "aceptada", dot]]
ppResult False = prettyPrint $ sep [pretty "La palabra fue", cat [annotate (color Red) $ pretty "rechazada", dot]]

ppVerboseGraphic :: MonadPDA m => FilePath -> m ()
ppVerboseGraphic f = doIfVerbose go f
                     where
                      go f = do hSep <- gethSep
                                vSep <- getvSep
                                dpi <- getDpi
                                tr <- getTransparentBg
                                prettyPrint $ hsep [verbose2doc "Se guardó el autómata graficado al archivo",
                                                    pretty f,
                                                    pretty "con separación horizontal",
                                                    pretty hSep,
                                                    pretty "separación vertical",
                                                    pretty vSep,
                                                    pretty "DPI",
                                                    pretty dpi,
                                                    pretty "y",
                                                    pretty $ if tr then "con" else "sin",
                                                    pretty "fondo transparente."]

ppErrorStackSy :: MonadPDA m => Symbol -> m ()
ppErrorStackSy sy = doIfVerbose prettyPrint (sep [sep [verbose2doc "En el alfabeto de pila no se encuentra el caracter",
                                                       stackSymbol2doc sy,
                                                       pretty "del"],
                                                 pretty "alfabeto de entrada."])
                                                  
ppErrorAccState :: MonadPDA m => State -> m ()
ppErrorAccState st = doIfVerbose prettyPrint (sep [verbose2doc "El estado de aceptación",
                                                   state2doc st,
                                                   pretty "no está dado como un estado."])

ppErrorTrInSy :: MonadPDA m => Transition -> Symbol -> m () 
ppErrorTrInSy t sy = doIfVerbose prettyPrint (sep [sep [verbose2doc "En la transición",
                                                        transition2doc t,
                                                        pretty "el caracter",
                                                        inputSymbol2doc sy,
                                                        pretty "no pertenece también al"],
                                                       pretty "alfabeto de entrada."])

ppErrorTrSkSy :: MonadPDA m => Transition -> Symbol -> m () 
ppErrorTrSkSy t sy = doIfVerbose prettyPrint (sep [sep [verbose2doc "En la transición",
                                                        transition2doc t,
                                                        pretty "el caracter",
                                                        stackSymbol2doc sy,
                                                        pretty "no pertenece al"],
                                                       pretty "alfabeto de pila."])

ppErrorTrState :: MonadPDA m => Transition -> State -> m ()
ppErrorTrState t st = doIfVerbose prettyPrint $ (sep [verbose2doc "En la transición",
                                                      transition2doc t,
                                                      pretty "el estado",
                                                      state2doc st,
                                                      pretty "no es un estado válido."])

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

prettyPrint :: MonadPDA m => Doc AnsiStyle -> m ()
prettyPrint = printPDA . render