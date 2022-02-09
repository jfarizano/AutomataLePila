module Graphic ( graphicPDA ) where

import Control.Monad.Trans ( lift, liftIO )

import System.FilePath ( takeExtension )

import Data.Text.Lazy ( pack )
import Data.Maybe ( fromJust )
import Data.List ( find )
import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Lang
import Monad
import Global
import PPrint

-- | Recibe la ruta de un archivo, grafica el autómata actual y lo exporta
-- al archivo dado.
graphicPDA :: MonadPDA m => FilePath -> m ()
graphicPDA f = do b <- liftIO $ isGraphvizInstalled
                  if b
                  then do let ext = takeExtension f
                          parsed <- parseFormat ext
                          case parsed of
                            Nothing -> failPDA "Extensión de archivo incorrecta."
                            Just format -> do au <- getActualPDA
                                              dot <- pda2dot au
                                              liftIO $ runGraphviz dot format f
                                              ppVerboseGraphic f
                                              ppOpDone "El archivo fue guardado."
                                              return ()                                              
                  else failPDA "Paquete GraphViz no instalado."


-- | Convierte el autómata dado al tipo DotGraph usado por la librería.
pda2dot :: MonadPDA m => Automaton -> m (DotGraph Node)
pda2dot au = do params <- dotParams au
                return $ graphToDot params (pda2graph au)

-- | Convierte el autómata dado a un tipo de grafo intermedio en el proceso.
pda2graph :: Automaton -> Gr State String
pda2graph au = mkGraph nodes edges
               where
                 st = states au
                 tr = transitions au
                 findNode s = fst $ fromJust $ find (\(_, x) -> x == s) nodes
                 nodes = zip [0..] ("" : st)
                 edges' = map (\(st0, sy0, sy1, sy2, st1) -> (findNode st0, findNode st1, (createLabel sy0 sy1 sy2))) tr
                 edges = if null edges' then edges' else ((0, 1, "") : edges')

-- | Da los parámetros utilizados al convertir el grafo intermedio a DotGraph.
dotParams :: MonadPDA m => Automaton -> m (GraphvizParams Node State String () String)
dotParams au = do global <- gStyle
                  return $ nonClusteredParams { 
  globalAttributes = global,
  fmtNode = \(n, label) -> ([Label (StrLabel (pack label)), 
                             Shape $ if label `elem` accStates au then DoubleCircle 
                                                                  else Circle]
                             ++ if n == 0 then [Style $ [SItem Invisible []], NodeSep 0,
                                                Height 0, Width 0, Margin $ DVal 0]
                                          else []),
  fmtEdge = \(n, n', label) -> [Label (StrLabel (pack label))]}

-- | Devuelve los atributos globales del grafo.
gStyle :: MonadPDA m => m [GlobalAttributes]
gStyle = do hSep <- gethSep
            vSep <- getvSep
            dpi <- getDpi
            tr <- getTransparentBg
            return $ [GraphAttrs [NodeSep vSep, RankSep [hSep],
                                  Size $ GSize 8 (Just 5) True, DPI dpi,
                                  RankDir FromLeft,
                                  BgColor $ if tr then [toWC $ X11Color Transparent]  
                                                  else [toWC $ X11Color White]],
                      NodeAttrs  [textLabel $ pack "\\N"],
                      EdgeAttrs  [color Black]]

-- | Crea la etiqueta utilizada en las aristas de las transiciones.
createLabel :: Char -> Char -> Char -> String
createLabel a b c = a' ++ ";" ++ b' ++ ";" ++ c'
            where
              a' = char2label a
              b' = char2label b
              c' = char2label c

-- | Transformar Char a String, si el char dado es el del lambda devuelve
-- el lambda en texto.
char2label :: Char -> String
char2label c = if c == '\955' then "λ" else [c]

-- | Lee el tipo del archivo de salida y devuelve el mismo pero en el tipo 
-- correspondiente utilizado por la librería.
parseFormat :: MonadPDA m => FilePath -> m (Maybe GraphvizOutput)
parseFormat ext | ext == ".png" = return $ Just Png
                | ext == ".jpeg" = return $ Just Jpeg
                | ext == ".jpg" = return $ Just Jpeg
                | ext == ".pdf" = return $ Just Pdf
                | ext == ".svg" = return $ Just Svg
                | ext == ".dot" = return $ Just Canon
                | otherwise = return Nothing