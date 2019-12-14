{-# LANGUAGE OverloadedStrings #-}

import Asterius.JSRun.Main
import Asterius.JSRun.NonMain
import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  callProcess "ahc-link" $
    [ "--input-hs",
      "test/nomain/NoMain.hs",
      "--output-ir",
      "--full-sym-table",
      "--ghc-option=-no-hs-main",
      "--extra-root-symbol=NoMain_x_closure"
    ]
      <> args
  m <- decodeFile "test/nomain/NoMain.unlinked.bin"
  withJSSession defJSSessionOpts {nodeStdErrInherit = True} $ \s -> do
    (i, rts_val, req_val, mod_val) <-
      newAsteriusInstanceNonMain
        s
        "test/nomain/NoMain"
        ["NoMain_x_closure"]
        m
    hsInit s i
    touch s i
    ps <- eval s $ deRefJSVal i <> ".persistentState"
    i' <-
      eval s $
        deRefJSVal rts_val
          <> ".newAsteriusInstance(Object.assign("
          <> deRefJSVal req_val
          <> ",{module:"
          <> deRefJSVal mod_val
          <> ",persistentState:"
          <> deRefJSVal ps
          <> "}))"
    touch s i'

touch :: JSSession -> JSVal -> IO ()
touch s i = do
  let x_closure = deRefJSVal i <> ".symbolTable.NoMain_x_closure"
      x_tid =
        "await " <> deRefJSVal i <> ".exports.rts_eval(" <> x_closure <> ")"
      x_ret = deRefJSVal i <> ".exports.getTSOret(" <> x_tid <> ")"
      x_sp = deRefJSVal i <> ".exports.rts_getStablePtr(" <> x_ret <> ")"
      x_val' = deRefJSVal i <> ".getJSVal(" <> x_sp <> ")"
      x_val = "(async () => " <> x_val' <> ")()"
  x <- eval s x_val
  LBS.putStr x
