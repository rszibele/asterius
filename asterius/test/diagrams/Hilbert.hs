{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Rendering.SVG
import Asterius.Types
import Graphics.Svg.Core (Element)
import Data.Coerce
import Lucid.Svg (Svg (..), prettyText)

hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

example = frame 1 . lw medium . lc darkred
                  . strokeT $ hilbert 5


foreign import javascript "document.body.appendChild(function () { const d = document.createElement('div'); d.innerHTML = ${1}; return d;}())" showSVG' :: JSVal -> IO ()
--foreign import javascript "console.log(${1})" showSVG' :: JSVal -> IO ()

main = do
    let
      showSVG s = showSVG' (coerce (toJSString (show s)))
      dia = (example :: Diagram B)
      opts = SVGOptions
         { _size            = dims2D 400 400
         , _svgDefinitions  = Nothing
         , _idPrefix        = mempty
         , _svgAttributes   = []
         , _generateDoctype = False
         }
      svg = renderDia SVG opts dia
    showSVG svg
