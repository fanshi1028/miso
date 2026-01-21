-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Run
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Support for running and live-reloading of miso applications.
----------------------------------------------------------------------------
module Miso.Run
  ( -- ** Live reload
    run
  , reload
  ) where
-----------------------------------------------------------------------------
import           Miso.String
import           Miso.DSL
#if !defined(WASM) && !GHCJS_BOTH
import           Data.Maybe
import           System.Environment
import           Text.Read
import           Language.Javascript.JSaddle hiding (jsg, (!))
import qualified Language.Javascript.JSaddle.Warp as J
import           Network.Wai.Middleware.Static (static)
import           Network.Wai.Handler.Warp (defaultSettings, setTimeout, setPort, runSettings)
import           Network.WebSockets (defaultConnectionOptions)
import           Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleOr, jsaddleAppWithJs, jsaddleJs)
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar
import           Miso.DSL.FFI
#endif
-----------------------------------------------------------------------------
-- | Entry point for a miso application.
--
run
  :: IO ()
  -- ^ An t'IO' action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
#ifdef VANILLA
run action = do
  port <- fromMaybe 8008 . (readMaybe =<<) <$> lookupEnv "PORT"
  isGhci <- (== "<interactive>") <$> getProgName
  putStrLn $ "Running on port " <> show port <> "..."
  if isGhci
    then do
  -- | Start or restart the server, with a static Middleware policy.
  --
  -- dmj: This is like @debug@ from `jsaddle-warp`, except it uses a static
  -- middleware for static file hosting.
  --
  -- This means that usage of `url('mario.png')` will "just work" when developing
  -- from GHCi.
  --
      debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
          jsaddleOr
            defaultConnectionOptions
            (registerContext >> askJSM >>= liftIO . putMVar currentJSContext >> liftIO action >> syncPoint)
            (static $ withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
    else
      runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (liftIO action >> syncPoint)
        (static J.jsaddleApp)
#else
run = id
#endif
-----------------------------------------------------------------------------
-- | Like 'run', but clears the <body> and <head> on each reload.
--
-- Meant to be used with WASM browser mode
--
-- @since 1.9.0.0
reload
  :: IO ()
  -- ^ A JSM action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
reload action = run (clear >> action)
  where
    clear :: IO ()
    clear = do
      body_ <- jsg "document" ! ("body" :: MisoString)
      setField body_ "innerHTML" ("" :: MisoString)
      head_ <- jsg "document" ! ("head" :: MisoString)
      setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
