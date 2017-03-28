{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-orphans #-}

module Main where

import           GHC.Generics
import           Control.Exception
import qualified Control.Exception                as EX
import qualified Data.HashMap.Lazy                as HM
import           Data.Semigroup
import           Options.Applicative              as OA
import           Prelude.Unicode
import           Text.Show.Pretty
import           Text.Printf                         (printf)
import           Text.Show.Unicode
import           Youtrack

main ∷ IO ()
main = do
  params ← get_params
  iss_orelse ← either_exception_or $ do
    (yt, projects) ← yt_connect params
    iss            ← yt_issues yt projects (Filter $ printf "has: comments") 1
    pure (yt, iss)
  (_, iss) ← case iss_orelse of
               Right pr_iss → pure pr_iss
               Left ex      → error $ printf "YT Project not accessible due to service access failure: %s" $ show ex
  case iss of
    []    → error "No issues with comments in whole YouTrack repository, showcase unavailable."
    (issue@Issue{..}:_) → do
      let Project{..} = issue_project
      putStrLn $ ppShow $ issue
      comments ← projectRequest issue_project issue $ RIssueComments project_alias issue_id
      case comments of
        [] → error "Youtrack returned a comment-free issue for a comment-only issue list request, showcase unavailable."
        (Comment{..}:_) → do
          printf "=========== Comment # %s, by %s, at %s\n" comm_id (show comm_author) (show comm_created)
          uprint comm_text

deriving instance (Show Issue)
deriving instance (Show Comment)
deriving instance (Show Description)
deriving instance (Show Hours)
deriving instance (Show Link)
deriving instance (Show State)
deriving instance (Show Summary)
deriving instance (Show Tag)
deriving instance (Show Type)
deriving instance (Show Priority)
deriving instance (Show IId)
instance (Show Project) where show x = show ∘ fromPName $ project_name x
deriving instance (Show PAlias)
deriving instance (Show PName)
deriving instance (Show YT)
instance (Show Member)  where show x = show ∘ fromMLogin $ member_login x
deriving instance (Show MLogin)
deriving instance (Generic YT)

yt_connect ∷ Params → IO (YT, ProjectDict)
yt_connect Params{..} = do
  let preyt = YT { ytHostname    = param_server
                 , ytRestSuffix  = param_restprefix
                 , ytLogin       = MLogin param_login
                 , ytWreqOptions = (⊥)
                 , ytJar         = (⊥) }

  yt       ← ytConnect preyt (SSLOptions "" param_server_cacert) Nothing
  projects ← ytRequest yt yt RProjectAll

  let projdict = HM.fromList [ (project_alias, p)
                             | p@Project{..} ← projects ]

  pure $ (yt, projdict)

yt_issues ∷ YT → ProjectDict → Filter → Int → IO [Issue]
yt_issues yt pdict ifilter ilimit = do
  ytRequest yt pdict ∘ RIssue ifilter ilimit
  $ Field <$> ["projectShortName", "numberInProject", "Type", "summary", "created", "reporterName"]

either_exception_or ∷ IO a → IO (Either SomeException a)
either_exception_or ioaction =
    let handler (e ∷ SomeException) = pure $ Left e
    in EX.handle handler $ fmap Right $ evaluate =<< ioaction

data Params where
    Params ∷
        { param_server        ∷ String
        , param_restprefix    ∷ String
        , param_server_cacert ∷ String
        , param_login         ∷ String
        } → Params
    deriving (Show)

get_params ∷ IO Params
get_params =
  customExecParser
    (prefs $ disambiguate <> showHelpOnError)
    (info
     (helper <*>
      (Params
       <$> strOption (help "Youtrack instance"
                     <> long "server"  <> metavar "HOSTNAME"   <> mempty)
       <*> strOption (help "Youtrack instance REST URL prefix"
                     <> long "rest-prefix" <> metavar "PREFIX" <> value "/rest")
       <*> strOption (help "CA certificate that validates server"
                     <> long "cacert"  <> metavar "FILE"       <> mempty)
       <*> strOption (help "YouTrack login"
                     <> long "login"   <> metavar "LOGIN"      <> mempty)))
     (  fullDesc
     <> progDesc "Perform a test query against some Youtrack instance."
     <> header   "youtrack-test" ))
