{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

#if (__GLASGOW_HASKELL__ > 710)
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#   define HASCALLSTACK HasCallStack =>
#else
#   define HASCALLSTACK
#endif
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-do-bind #-}

module Youtrack
    (
      module Youtrack.Exchanges
    , module Youtrack.Names
    , module Youtrack.Types

    -- * YT access
    , ytConnect, SSLOptions(..)

    -- * Querying
    , ytRequestRaw
    , ytDecode
    , ytRequest

    -- Slightly higher level:
    , projectRequest

    -- Debug oriented:
    , ytSaveJSON, ytLoadJSON, ytLoadJSONValue, ytLoadRequest
    )
where


-- Base imports
import           Control.Monad.Reader
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
#if (__GLASGOW_HASKELL__ > 710)
import           GHC.Stack                   (HasCallStack)
#endif
import           Prelude.Unicode

-- External imports
import           Control.Lens         hiding (from, Context)
import           Data.Aeson                  (FromJSON (..), Value)
import           Network.Wreq                (FormParam((:=)))
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)

import qualified Data.Aeson                   as AE
import qualified Data.ByteString.Lazy.UTF8    as BL
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.UTF8         as BS
import qualified Network.HTTP.Client.OpenSSL  as SSL
import qualified Network.Wreq                 as WR
import qualified OpenSSL.Session              as SSL

-- Debug imports
import           Text.Printf                 (printf)

-- Local imports
import           Authinfo                    (getPassword)
import           Youtrack.Exchanges
import           Youtrack.Names
import           Youtrack.Types


-- * SSL shenanigans
data SSLOptions = SSLOptions {
      optionsClientCert ∷ FilePath
    , optionsCaCert     ∷ FilePath
    } deriving (Eq, Show)

setupSSLCtx ∷ SSLOptions → IO SSLContext
setupSSLCtx (SSLOptions _ caCert) = do
  ctx ← SSL.context
  SSL.contextSetCertificateFile ctx caCert
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv3
  pure ctx


-- * 0. Access
ytConnect ∷ HASCALLSTACK YT → SSLOptions → Maybe String → IO YT
ytConnect yt@YT{..} ssl_options maipath = do
  maypass ← getPassword maipath ytHostname $ fromMLogin ytLogin
  let password = fst ∘ (flip fromMaybe) maypass $
                 error $ printf "ERROR: no password for username '%s' on host '%s' in %s." (fromMLogin ytLogin) ytHostname $
                           case maipath ∷ Maybe FilePath of
                             Nothing → "standard authinfo file" ∷ String
                             Just x  → printf "authinfo file '%s'" x
  let mkOpts c  = WR.defaults & ( (WR.manager .~ Left (SSL.opensslManagerSettings c))
                                ∘ (WR.auth ?~ WR.basicAuth (BS.fromString ∘ fromMLogin $ ytLogin) (BS.fromString password)))
      wreq_opts = mkOpts (setupSSLCtx ssl_options)
  withOpenSSL $ do
    r ← WR.postWith wreq_opts
        ("https://" <> ytHostname <> ytRestSuffix <> "/user/login")
        ["login" := (fromMLogin ytLogin), "password" := password]
    pure $ yt { ytWreqOptions = wreq_opts
              , ytJar         = Just $ r ^. WR.responseCookieJar }

-- * 1. Request
ytRequestRaw ∷ (Exchange y) ⇒ YT → Request y → IO BL.ByteString
ytRequestRaw YT{..} req = do
  let url    = "https://" <> ytHostname <> ytRestSuffix <> (fromURLPath $ request_urlpath req)
      params = request_params req
      opts   = ytWreqOptions & (WR.header "Accept" .~ ["application/json"]) & params
  -- printf "--> %s %s\n" url (show $ opts ^. WR.params )
  r ← withOpenSSL $
      case request_type req of
        Get  → WR.getWith  opts url
        Post → WR.postWith opts url $ request_post_args req
  -- printf "<-- %s\n" $ show $ r ^. WR.responseBody
  pure $ r ^. WR.responseBody

-- * 2. Decode
ytDecode ∷ HASCALLSTACK (FromJSON r) ⇒ BL.ByteString → IO r
ytDecode bs = do
  case AE.eitherDecode bs of
    Left e     → error e
    Right resp → pure resp


-- * Main API
ytRequest ∷ HASCALLSTACK (Exchange y) ⇒ YT → Context y → Request y → IO (Response y)
ytRequest yt ctx req = do
  raw ← ytRequestRaw yt req
  ctxless ← ytDecode raw
  pure $ fmap ((flip runReader) ctx) ctxless

projectRequest ∷ HASCALLSTACK (Exchange y) ⇒ Project → Context y → Request y → IO (Response y)
projectRequest Project{..} ctx req =
  ytRequest project_yt ctx req


-- * Debug-oriented stuff

-- ..yeah, code duplication, niasilil..
ytSaveJSON ∷ (Exchange y) ⇒ YT → Request y → IO ()
ytSaveJSON yt req =
    ytRequestRaw yt req >>=
    BL.writeFile (take 64 $ "yt-request-" <> show req)

ytLoadJSON ∷ (Exchange y, FromJSON r) ⇒ Request y → IO r
ytLoadJSON req =
    BL.readFile (take 64 $ "yt-request-" <> show req) >>=
    ytDecode

ytLoadJSONValue ∷ (Exchange y) ⇒ Request y → IO Value
ytLoadJSONValue = ytLoadJSON

ytLoadRequest ∷ (Exchange y) ⇒ Context y → Request y → IO (Response y)
ytLoadRequest ctx req = do
    raw ← BL.readFile (take 64 $ "yt-request-" <> show req)
    ctxless ← ytDecode raw
    pure $ fmap ((flip runReader) ctx) ctxless
