{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Ld
  ( LinkTask (..),
    linkModules,
    linkExeInMemory,
    linkExe,
    rtsUsedSymbols,
  )
where

import Asterius.Ar
import Asterius.Builtins
import Asterius.Internals
import Asterius.Internals.Containers
import Asterius.Resolve
import Asterius.Types
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Either
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable
import Prelude hiding (IO)

data LinkTask
  = LinkTask
      { progName, linkOutput :: FilePath,
        linkObjs, linkLibs :: [FilePath],
        linkModule :: AsteriusModule,
        debug, gcSections, verboseErr :: Bool,
        outputIR :: Maybe FilePath,
        rootSymbols, exportFunctions :: [AsteriusEntitySymbol]
      }
  deriving (Show)

loadTheWorld :: LinkTask -> IO AsteriusModule
loadTheWorld LinkTask {..} = do
  lib <- mconcat <$> for linkLibs loadAr
  objrs <- for linkObjs tryDecodeFile
  let objs = rights objrs
  evaluate $ linkModule <> mconcat objs <> lib

-- | The *_info are generated from Cmm using the INFO_TABLE macro.
-- For example, see StgMiscClosures.cmm / Exception.cmm
rtsUsedSymbols :: Set AsteriusEntitySymbol
rtsUsedSymbols =
  Set.fromList
    [ "barf",
      "base_AsteriusziTopHandler_runMainIO_closure",
      "base_AsteriusziTypes_makeJSException_closure",
      "base_GHCziPtr_Ptr_con_info",
      "ghczmprim_GHCziTypes_Czh_con_info",
      "ghczmprim_GHCziTypes_Dzh_con_info",
      "ghczmprim_GHCziTypes_False_closure",
      "ghczmprim_GHCziTypes_Izh_con_info",
      "ghczmprim_GHCziTypes_True_closure",
      "ghczmprim_GHCziTypes_Wzh_con_info",
      "ghczmprim_GHCziTypes_ZC_con_info",
      "ghczmprim_GHCziTypes_ZMZN_closure",
      "MainCapability",
      "Main_main_closure",
      "stg_ARR_WORDS_info",
      "stg_BLACKHOLE_info",
      "stg_WHITEHOLE_info",
      "stg_IND_info",
      "stg_DEAD_WEAK_info",
      "stg_marked_upd_frame_info",
      "stg_NO_FINALIZER_closure",
      "stg_raise_info",
      "stg_raisezh",
      "stg_returnToStackTop",
      "stg_STABLE_NAME_info",
      "stg_WEAK_info"
    ]

linkModules ::
  LinkTask -> AsteriusModule -> (AsteriusModule, Module, LinkReport)
linkModules LinkTask {..} m =
  linkStart
    debug
    gcSections
    verboseErr
    ( rtsAsteriusModule
        defaultBuiltinsOptions
          { Asterius.Builtins.progName = progName,
            Asterius.Builtins.debug = debug,
            Asterius.Builtins.jsvalConInfo = m_jsval_con_info
          }
        <> m
    )
    ( Set.unions
        [ Set.fromList rootSymbols,
          rtsUsedSymbols,
          Set.fromList
            [ AsteriusEntitySymbol {entityName = internalName}
              | FunctionExport {..} <- rtsFunctionExports debug
            ]
        ]
    )
    exportFunctions
  where
    f sym _
      | "asteriuszmprelude"
          `BS.isPrefixOf` buf
          && "AsteriusziPrim_JSVal_con_info"
          `BS.isSuffixOf` buf =
        Just sym
      | otherwise =
        Nothing
      where
        buf = SBS.fromShort $ coerce sym
    m_jsval_con_info = find f (staticsMap m)

linkExeInMemory :: LinkTask -> IO (AsteriusModule, Module, LinkReport)
linkExeInMemory ld_task = do
  final_store <- loadTheWorld ld_task
  evaluate $ linkModules ld_task final_store

linkExe :: LinkTask -> IO ()
linkExe ld_task@LinkTask {..} = do
  (pre_m, m, link_report) <- linkExeInMemory ld_task
  encodeFile linkOutput (m, link_report)
  case outputIR of
    Just p -> encodeFile p pre_m
    _ -> pure ()
