{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (cycle)

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Rendering.SVG
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Text (Text)
import Data.Coerce
import Asterius.Types
import Lucid.Svg (Svg (..), prettyText)
import Graphics.Svg.Core (Element)

blank :: Diagram B
blank = lw none $ square 16

makeTile :: [[P2 Double]] -> Diagram B
makeTile = showOrigin . lw thin . centerXY . mconcat . map fromVertices where

markingsP, markingsQ, markingsR, markingsS :: [[P2 Double]]

data Picture
  = Blank
  | Single [[P2 Double]]
  | Rot Picture
  | Cycle Picture
  | HPair Picture Picture
  | VPair Picture Picture
  | Quartet Picture Picture Picture Picture
  | SkewQuartet Picture Picture Picture Picture -- left column and bottom row half size

rot p = rotateBy (1/4) p
cycle p = quartet (rot p) p (rot $ rot p) (rot $ rot $ rot p)
hpair p q = scaleX (1/2) $ centerXY (p ||| q)
vpair p q = scaleY (1/2) $ centerXY (p === q)
quartet p q r s = scale (1/2) $ centerXY ((p ||| q) === (r ||| s))
skewquartet p q r s = scale (1/3) $ centerXY ((scaleY 2 p ||| scale 2 q)
                                              === (r ||| scaleX 2 s))

drawPicture :: Picture -> Diagram B
drawPicture Blank = blank
drawPicture (Single m) = makeTile m
drawPicture (Rot p) = rot (drawPicture p)
drawPicture (Cycle p) = cycle (drawPicture p)
drawPicture (HPair p q) = hpair (drawPicture p) (drawPicture q)
drawPicture (VPair p q) = vpair (drawPicture p) (drawPicture q)
drawPicture (Quartet p q r s)
  = quartet (drawPicture p) (drawPicture q) (drawPicture r) (drawPicture s)
drawPicture (SkewQuartet p q r s)
  = skewquartet (drawPicture p) (drawPicture q) (drawPicture r) (drawPicture s)

prune :: Double -> Picture -> Picture
prune s p = prune' (s,s) p where
  prune' (x,y) p | min x y < 1 = Blank -- cut off when either factor <1
  prune' (x,y) Blank = Blank
  prune' (x,y) (Single m) = Single m
  prune' (x,y) (Rot p) = Rot (prune' (y,x) p)
  prune' (x,y) (Cycle p) = Cycle (prune' (x/2,y/2) p) -- assumes x==y
  prune' (x,y) (HPair p q) = HPair (prune' (x/2,y) p) (prune' (x/2,y) q)
  prune' (x,y) (VPair p q) = VPair (prune' (x,y/2) p) (prune' (x,y/2) q)
  prune' (x,y) (Quartet p q r s)
    = Quartet (prune' x2y2 p) (prune' x2y2 q) (prune' x2y2 r) (prune' x2y2 s)
      where x2y2 = (x/2,y/2)
  prune' (x,y) (SkewQuartet p q r s)
    = SkewQuartet (prune' (x3,y32) p) (prune' (x32,y32) q)
                  (prune' (x3,y3) r) (prune' (x32,y3) s)
      where (x3,y3,x32,y32) = (x/3,y/3,x3*2,y3*2)

fishP = Single markingsP
fishQ = Single markingsQ
fishR = Single markingsR
fishS = Single markingsS

fishT = Quartet fishP fishQ fishR fishS
fishU = Cycle (Rot fishQ)

corner = SkewQuartet p q r s where
  p = VPair p' fishT
  p' = HPair (Rot s) p
  q = Quartet p' q fishU s'
  r = fishQ
  s = HPair fishT s'
  s' = VPair s (Rot (Rot (Rot p)))

squarelimit = Cycle corner

example = drawPicture (prune 24 squarelimit) `atop` square 14.5
        # fc darkseagreen
        # opacity 0.5

markingsP = [
  [ (4^&4), (6^&0) ],
  [ (0^&3), (3^&4), (0^&8), (0^&3) ],
  [ (4^&5), (7^&6), (4^&10), (4^&5) ],
  [ (11^&0), (10^&4), (8^&8), (4^&13), (0^&16) ],
  [ (11^&0), (14^&2), (16^&2) ],
  [ (10^&4), (13^&5), (16^&4) ],
  [ (9^&6), (12^&7), (16^&6) ],
  [ (8^&8), (12^&9), (16^&8) ],
  [ (8^&12), (16^&10) ],
  [ (0^&16), (6^&15), (8^&16), (12^&12), (16^&12) ],
  [ (10^&16), (12^&14), (16^&13) ],
  [ (12^&16), (13^&15), (16^&14) ],
  [ (14^&16), (16^&15) ]
  ]

markingsQ = [
  [ (2^&0), (4^&5), (4^&7) ],
  [ (4^&0), (6^&5), (6^&7) ],
  [ (6^&0), (8^&5), (8^&8) ],
  [ (8^&0), (10^&6), (10^&9) ],
  [ (10^&0), (14^&11) ],
  [ (12^&0), (13^&4), (16^&8), (15^&10), (16^&16), (12^&10), (6^&7), (4^&7), (0^&8) ],
  [ (13^&0), (16^&6) ],
  [ (14^&0), (16^&4) ],
  [ (15^&0), (16^&2) ],
  [ (0^&10), (7^&11) ],
  [ (9^&12), (10^&10), (12^&12), (9^&12) ],
  [ (8^&15), (9^&13), (11^&15), (8^&15) ],
  [ (0^&12), (3^&13), (7^&15), (8^&16) ],
  [ (2^&16), (3^&13) ],
  [ (4^&16), (5^&14) ],
  [ (6^&16), (7^&15) ]
  ]

markingsR = [
  [ (0^&12), (1^&14) ],
  [ (0^&8), (2^&12) ],
  [ (0^&4), (5^&10) ],
  [ (0^&0), (8^&8) ],
  [ (1^&1), (4^&0) ],
  [ (2^&2), (8^&0) ],
  [ (3^&3), (8^&2), (12^&0) ],
  [ (5^&5), (12^&3), (16^&0) ],
  [ (0^&16), (2^&12), (8^&8), (14^&6), (16^&4) ],
  [ (6^&16), (11^&10), (16^&6) ],
  [ (11^&16), (12^&12), (16^&8) ],
  [ (12^&12), (16^&16) ],
  [ (13^&13), (16^&10) ],
  [ (14^&14), (16^&12) ],
  [ (15^&15), (16^&14) ]
  ]

markingsS = [
  [ (0^&0), (4^&2), (8^&2), (16^&0) ],
  [ (0^&4), (2^&1) ],
  [ (0^&6), (7^&4) ],
  [ (0^&8), (8^&6) ],
  [ (0^&10), (7^&8) ],
  [ (0^&12), (7^&10) ],
  [ (0^&14), (7^&13) ],
  [ (8^&16), (7^&13), (7^&8), (8^&6), (10^&4), (16^&0) ],
  [ (10^&16), (11^&10) ],
  [ (10^&6), (12^&4), (12^&7), (10^&6) ],
  [ (13^&7), (15^&5), (15^&8), (13^&7) ],
  [ (12^&16), (13^&13), (15^&9), (16^&8) ],
  [ (13^&13), (16^&14) ],
  [ (14^&11), (16^&12) ],
  [ (15^&9), (16^&10) ]
  ]


-- foreign import javascript "document.createElement('div').innerHtml = ${1}" showSVG' :: JSVal -> IO ()
foreign import javascript "console.log(${1})" showSVG' :: JSVal -> IO ()


main = do
    let
      showSVG s = showSVG' (coerce (toJSString (show s)))
      dia = example
      opts = SVGOptions
         { _size            = dims2D w h
         , _svgDefinitions  = Nothing
         , _idPrefix        = mempty
         , _svgAttributes   = []
         , _generateDoctype = False
         }
      (V2 w h) = size dia
      svg = renderDia SVG opts dia
    showSVG svg
