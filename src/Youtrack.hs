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
#   define HASCALLSTACK HasCallStack =>
#else
#   define HASCALLSTACK
#endif
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-do-bind #-}

module Youtrack
    (
      module Names
    -- * YT access
    , Access(..), YT, SSLOptions(..)
    , ytConnect

    -- * Data model
    , Member(..), Project(..), Issue(..), WorkItem(..)
    --
    , MLogin(..), MFullName (..)
    , PAlias(..), PName(..), PDate(..)
    , IId(..), Type(..), Priority(..), Summary(..), Description(..), State(..), Tag(..), Link(..)
    , Created(..), Resolved(..), Updated(..)
    , Performed(..), Activity(..)
    , Hours(..)

    -- * Request model
    , EProjectAll(..), EIssueByProject(..), EIssue(..), EIssueTTWItem(..)
    , URLPath(..), ExchangeType(..)
    , Exchange(..)
    , Request(..)
    , Filter(..), Field(..)

    -- * Querying
    , ytRequestRaw
    , ytDecode
    , ytRequest
    , ytSaveJSON, ytLoadJSON
    , ytLoadJSONValue, ytLoadRequest
    )
where


-- Base imports
import           Control.Applicative         ((<|>))
import           Control.Monad               (join)
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           GHC.Exts                    (Constraint)
import           GHC.Generics                (Generic(Rep))
#if (__GLASGOW_HASKELL__ > 710)
import           GHC.Stack                   (HasCallStack)
#endif
import           Prelude.Unicode


-- Debug imports
import           Text.Printf                 (printf)
--import           Debug.Trace                 (trace)


-- External imports
import           Control.Lens         hiding (from)
import           Data.Aeson                  (FromJSON (..), Value, (.:))
import           Data.Aeson.Types            (Options (..))
import           Data.List.Split             (splitOn)
import           Data.Scientific             (Scientific)
import           Data.Time.LocalTime         (LocalTime (..), utcToLocalTime, hoursToTimeZone)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Network.HTTP.Client         (CookieJar)
import           Network.Wreq                (FormParam((:=)))
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext)
import           Safe                        (headMay)

import qualified Data.Aeson                   as AE
import qualified Data.Aeson.Types             as AE
import qualified Data.ByteString.Lazy.UTF8    as BL
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.UTF8         as BS
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Network.HTTP.Client.OpenSSL  as SSL
import qualified Network.Wreq                 as WR
import qualified OpenSSL.Session              as SSL
import qualified System.Environment
import qualified Text.Parser.Char             as P
import qualified Text.Parser.Combinators      as P
import qualified Text.Parser.Token            as P
import qualified Text.Trifecta.Parser         as P


-- Local imports
import           Names


-- * Credentials storage: ~/.authinfo (ex-hsauthinfo) -- TODO:
gettok ∷ (Monad p, P.TokenParsing p) ⇒ Bool → String → p a → p a
gettok lastp key val = do
  P.string key
  (P.skipSome $ P.oneOf " \t") P.<?> "non-breaking whitespace"
  ret ← val
  (if lastp then P.skipMany else P.skipSome) $ P.oneOf " \t"
  pure ret

readAuthInfoLine ∷ (Monad p, P.TokenParsing p) ⇒ p (String, String, String, Maybe String)
readAuthInfoLine = do
  P.whiteSpace
  machine  ←              gettok False "machine"  $ P.some $ P.alphaNum <|> P.oneOf "-."
  login    ←              gettok False "login"    $ P.some P.alphaNum
  mport    ← P.optional $ gettok False "port"     $ P.some P.alphaNum
  password ←              gettok True  "password" $ P.some $ P.noneOf " \t\n\r"
  pure (machine, login, password, mport)

readAuthInfo ∷ (Monad p, P.TokenParsing p) ⇒ p [(String, String, String, Maybe String)]
readAuthInfo = do
  lines' ← P.sepEndBy readAuthInfoLine (P.newline)
  P.whiteSpace
  P.eof
  pure lines'

readAuthInfoFile ∷ String → IO (Maybe [(String, String, String, Maybe String)])
readAuthInfoFile aipath = do
  P.parseFromFile readAuthInfo aipath

getPassword ∷ Maybe String → String → String → IO (Maybe (String, Maybe String))
getPassword maipath host user = do
  aipath ← case maipath of
             Nothing → fmap (<> "/.authinfo") $ System.Environment.getEnv "HOME"
             Just p  → pure p
  mainfo ← readAuthInfoFile aipath
  pure $ join $ fmap (fmap (\ (_,_,pass,port) -> (pass,port))
                     ∘
                      find (\ (h,u,_,_) -> h == host && u == user))
                mainfo


-- * SSL shenanigans
data SSLOptions = SSLOptions {
      optionsClientCert ∷ FilePath
    , optionsCaCert     ∷ FilePath
    } deriving (Eq, Show)

setupSSLCtx ∷ HASCALLSTACK SSLOptions → IO SSLContext
setupSSLCtx (SSLOptions _ caCert) = do
  ctx ← SSL.context
  SSL.contextSetCertificateFile ctx caCert
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption          ctx SSL.SSL_OP_NO_SSLv3
  pure ctx


-- * Accessing YouTrack
data Access where
    Access ∷ {
      hostname ∷ String
    , ssl_opts ∷ SSLOptions
    , login    ∷ String
    , password ∷ Maybe String
    } → Access

data YT where
    YT ∷ {
      access       ∷ Access
    , wreq_options ∷ WR.Options
    , jar          ∷ CookieJar
    } → YT

complete_yt_access ∷ HASCALLSTACK Access → Maybe String → IO Access
complete_yt_access yta maipath = do
    macc ← getPassword maipath (hostname yta) (login yta)
    case macc of
      Nothing        → error $ printf "ERROR: no password for username '%s' on host '%s'." (login yta) (hostname yta)
      Just (pass, _) → pure $ yta { password = Just pass }

ytConnect ∷ HASCALLSTACK Access → Maybe String → IO YT
ytConnect access maipath = do
  full_access ← complete_yt_access access maipath
  let (Access host ssl_opts login (Just password)) = full_access
  let mkOpts c  = WR.defaults & ( (WR.manager .~ Left (SSL.opensslManagerSettings c))
                                ∘ (WR.auth ?~ WR.basicAuth (BS.fromString login) (BS.fromString password)))
      wreq_opts = mkOpts (setupSSLCtx ssl_opts)
  withOpenSSL $ do
    r ← WR.postWith wreq_opts
        ("https://" <> host <> "/rest/user/login")
        ["login" := login, "password" := password]
    pure $ YT access wreq_opts $ r ^. WR.responseCookieJar


-- * Leaf types
newtype_from_JSON,recdrop1_from_JSON ∷ (Generic a, AE.GFromJSON (Rep a)) ⇒ Value → AE.Parser a
newtype_from_JSON  = AE.genericParseJSON (AE.defaultOptions { unwrapUnaryRecords = True })
recdrop1_from_JSON = AE.genericParseJSON (AE.defaultOptions { fieldLabelModifier = drop 1 })

newtype URLPath      = URLPath       { fromURLPath      ∷ String } deriving Show

newtype Hours        = Hours         { fromHours ∷ Int }           deriving (Eq, Generic, Ord, FromJSON)
newtype Filter       = Filter        { fromFilter ∷ String }       deriving (Generic)
newtype Field        = Field         { field ∷ String }            deriving (Generic)

newtype PName        = PName         { fromPName ∷ String }        deriving (Generic)
instance FromJSON      PName                                       where parseJSON = newtype_from_JSON
newtype PAlias       = PAlias        { fromPAlias ∷ String }       deriving (Generic, Read)
instance FromJSON      PAlias                                      where parseJSON = newtype_from_JSON
-- newtype PVersion     = PVersion   { fromPVersion ∷ String }     deriving (Generic)
-- instance FromJSON      PVersion                                 where parseJSON = newtype_from_JSON
-- newtype PVersionList = PVersionList [String]                    deriving (Generic)
-- instance FromJSON      PVersionList                             where
--     parseJSON (AE.Null)      = pure $ PVersionList []
--     parseJSON v@(AE.Array _) = AE.genericParseJSON AE.defaultOptions v
--     parseJSON v              = fail $ printf "unexpected value for a version list: %s" (show v)
interpret_strdate ∷ T.Text → LocalTime
interpret_strdate = utcToLocalTime (hoursToTimeZone 4) ∘ posixSecondsToUTCTime ∘ fromIntegral ∘ (floor ∷ Double → Integer) ∘ ((/ 1000.0) ∷ Double → Double) ∘ read ∘ T.unpack
newtype PDate        = PDate         { fromPDate ∷ LocalTime }     deriving (Eq, Generic, Ord)
instance FromJSON      PDate                                       where parseJSON = AE.withText "date" $ \n → do
                                                                                       pure ∘ PDate $ interpret_strdate n
newtype Created      = Created       { fromCreated ∷ LocalTime }   deriving (Eq, Generic, Ord)
instance FromJSON      Created                                     where parseJSON = AE.withText "created date" $ \n → do
                                                                                       pure ∘ Created $ interpret_strdate n
newtype IId          = IId           { fromIId ∷ String }          deriving (Eq, Generic, Ord)
instance FromJSON      IId                                         where parseJSON = newtype_from_JSON
newtype Priority     = Priority      { fromPriority ∷ String }     deriving (Eq, Generic, Ord)
instance FromJSON      Priority                                    where parseJSON = newtype_from_JSON
newtype Resolved     = Resolved      { fromResolved ∷ LocalTime }  deriving (Eq, Generic, Ord)
instance FromJSON      Resolved                                    where parseJSON = AE.withText "resolved date" $ \n → do
                                                                                       pure ∘ Resolved $ interpret_strdate n
newtype State        = State         { fromState ∷ String }        deriving (Eq, Generic, Ord)
instance FromJSON      State                                       where parseJSON = newtype_from_JSON
newtype Tag          = Tag           { fromTag ∷ String }          deriving (Generic) -- XXX: should be derivable
instance FromJSON      Tag                                         where parseJSON = AE.withObject "issue tag" $ \o →
                                                                                     Tag <$> o .: "value"
newtype Summary      = Summary       { fromSummary ∷ String }      deriving (Eq, Generic, Ord)
instance FromJSON      Summary                                     where parseJSON = newtype_from_JSON
newtype Description  = Description   { fromDescription ∷ String }  deriving (Eq, Generic, Ord)
instance FromJSON      Description                                 where parseJSON = newtype_from_JSON
newtype Type         = Type          { fromType ∷ String  }        deriving (Eq, Generic, Ord)
instance FromJSON      Type                                        where parseJSON = newtype_from_JSON
newtype Updated      = Updated       { fromUpdated ∷ LocalTime }   deriving (Eq, Generic, Ord)
instance FromJSON      Updated                                     where parseJSON = AE.withText "updated date" $ \n → do
                                                                                       pure ∘ Updated $ interpret_strdate n
newtype MLogin       = MLogin        { fromMLogin ∷ String }       deriving (Eq, Generic, Ord)
instance FromJSON      MLogin                                      where
    parseJSON (AE.String s) = pure ∘ MLogin $ T.unpack s
    -- ,("author",Object (fromList [("ringId",String "58694c6b-d9cd-4ab1-bae4-e01db8516d44"),("url",String "https://gra-tracker.ptsecurity.com/rest/admin/user/skovalev"),("login",String "skovalev")]))
    parseJSON (AE.Object o) =
      case HM.lookup "login" o <|> HM.lookup "value" o of
        Just (AE.String s) → pure ∘ MLogin $ T.unpack s
        _                  → fail $ printf "Not a ∈ login object: %s" $ show o
    parseJSON o             = fail $ printf "Not a ∈ login object: %s" $ show o
newtype MFullName    = MFullName     { fromMFullName ∷ FullName }  deriving (Eq, Generic, Ord) -- XXX: should be derivable
instance FromJSON      MFullName                                   where parseJSON = AE.withObject "∈ full name" $ \o → do
                                                                                       str ← o .: "value"
                                                                                       let (familyname:givennametokens) = splitOn " " str
                                                                                           givenname = intercalate " " givennametokens
                                                                                       pure ∘ MFullName $ FullName (FamilyName familyname) (GivenName givenname)
-- ("worktype",Object (fromList [("url",String "https://gra-tracker.ptsecurity.com/rest/admin/timetracking/worktype/38-3"),("name",String "Bughunt"),("id",String "38-3"),("autoAttached",Bool False)]))
newtype Activity     = Activity      { fromActivity ∷ String }     deriving (Eq, Generic, Ord)
instance FromJSON      Activity                                    where parseJSON = AE.withObject "work type" $ \o →
                                                                                     Activity <$> o .: "name"

interpret_scidate ∷ Scientific → LocalTime
interpret_scidate = utcToLocalTime (hoursToTimeZone 4) ∘ posixSecondsToUTCTime ∘ fromIntegral ∘ (floor ∷ Scientific → Integer) ∘ (/ 1000)
newtype Performed    = Performed     { fromPerformed ∷ LocalTime } deriving (Generic, Eq, Ord)
instance FromJSON      Performed                                   where parseJSON = AE.withScientific "work date" $ \n → do
                                                                                       pure ∘ Performed $ interpret_scidate n
                                                                      -- Performed <$> (interpret_wdate $ o .: "name")

data Link =
     Link { _value ∷ String
          , _role  ∷ String
          , _type  ∷ String }
                                                                   deriving (Generic)
instance FromJSON   Link                                           where parseJSON = recdrop1_from_JSON


-- * JSON debug assists
abbrev_object ∷ Value → Value
abbrev_object (AE.Object omap) = AE.Object $ (flip HM.mapWithKey) omap
                                             (\k v → case k of
                                                       "name" → v
                                                       _      → abbrev_object v)
abbrev_object (AE.String s)   = AE.String $ T.take 3 s
abbrev_object (AE.Array  xs)  = AE.Array $ fmap abbrev_object xs
abbrev_object x               = x


-- * Data model

data Project =
    Project {
      project_alias         ∷ PAlias
    , project_name          ∷ PName
    , project_members       ∷ [Member] -- initially empty
    } deriving (Generic)

data Member =
    Member {
      member_login          ∷ MLogin
    , member_fullname       ∷ MFullName
    } deriving (Generic)

-- XXX: hard-coded assumptions about project issue structure
--      ..to be resolved by "2014 Bahr - Composing and Decomposing Data Types"
data Issue =
    Issue {
      issue_id              ∷ IId
    , issue_summary         ∷ Summary
    , issue_type            ∷ Type        --
    , issue_priority        ∷ Priority    --
    , issue_author          ∷ Member
    , issue_assignee        ∷ Maybe Member --
    , issue_state           ∷ State       --
    , issue_created         ∷ LocalTime
    , issue_resolved        ∷ Maybe LocalTime
    , issue_updated         ∷ LocalTime
    , issue_estimation      ∷ Maybe Hours  --
    , issue_description     ∷ Maybe Description
    , issue_votes           ∷ Int
    , issue_links           ∷ [Link]
    , issue_tags            ∷ [Tag]
    , issue_fields          ∷ HM.HashMap T.Text AE.Value
    } deriving (Generic)

data WorkItem =
    WorkItem {
      workitem_id           ∷ String
    , workitem_activity     ∷ Maybe Activity
    , workitem_date         ∷ LocalTime
    , workitem_author       ∷ Member
    , workitem_duration     ∷ Hours
    , workitem_description  ∷ Maybe String
    } deriving (Generic)

-- data Label (l ∷ Symbol) = Get
-- class Has a l b | a l → b where
--   fromL ∷ a → Label l → b
-- data Point = Point Int Int deriving Show
-- instance Has Point "x" Int where from (Point x _) _ = x
-- instance Has Point "y" Int where from (Point _ y) _ = y
-- example = from (Point 1 2) (Get :: Label "x")


-- * FromJSON

instance FromJSON    Project where { parseJSON
    = AE.withObject "Project" $
      \o → do
        project_alias        ← o .: "shortName"
        project_name         ← o .: "name"
        logins ∷ [MLogin]    ← o .: "assigneesLogin"
        names  ∷ [MFullName] ← o .: "assigneesFullName"
        let project_members = fmap (\(l, n) → Member l n) $ zip logins names
        pure Project{..}; }

value_map_lookup ∷ String → HM.HashMap T.Text AE.Value → T.Text → AE.Value
value_map_lookup desc hm key =
    HM.lookupDefault (error $ printf "missing %s: %s" desc $ show key) key hm

value_lookup ∷ String → T.Text → AE.Value → AE.Value
value_lookup desc key (AE.Object o) =
    value_map_lookup desc o key
value_lookup desc key val =
    error $ printf "while looking for key %s in %s: got non-object %s" (show key) desc $ show val

lookup_member ∷ [Member] → MLogin → Member
lookup_member ms ((flip find) ms ∘ (\l m → l ≡ member_login m) → Just m) = m
lookup_member _ ml = error $ printf "Couldn't find project ∈ with login name '%s'." $ fromMLogin ml

instance FromJSON ([Reader Project Issue]) where
    parseJSON = AE.withObject "Issue wrapper" $
       \o → case value_map_lookup "Issue list" o "issue" of
              AE.Array xs → forM (V.toList xs) parseJSON
              erx         → error $ printf "Not an issue list in the 'value' element of issue list."

instance FromJSON (Reader Project Issue) where
    parseJSON = AE.withObject "Issue" $
       \o → do
        let fields ∷ HM.HashMap T.Text AE.Value
            fields = case value_map_lookup "issue field list" o "field" of
                       AE.Array xs →
                           HM.fromList
                             [ ( case value_lookup "field name" "name" obj of
                                   AE.String t → t
                                   _           → error "Malformed field value: non-text 'name' field."
                               , value_lookup "field value" "value" obj )
                             | obj ← V.toList xs]
                       erx         →
                           error $ printf "Not a field list in the 'field' issue element, got %s instead." $ show erx
            missing ∷ String → String → String
            missing t v = printf "missing Issue %s: %s" t v
            field ∷ T.Text → (T.Text, Maybe Value)
            field fld = (,) fld $ HM.lookup fld fields
            mget ∷ (FromJSON a) ⇒ (T.Text, Maybe Value) → AE.Parser (Maybe a)
            mget (fld, mvalue) =
                case mvalue of
                  Just val
                      → case AE.fromJSON val of
                          AE.Success res' → pure res'
                          AE.Error   e    → fail $ printf "while parsing field %s: %s, obj=%s" fld e (show val)
                  _   → pure Nothing
            get ∷ (FromJSON a) ⇒ (T.Text, Maybe Value) → AE.Parser a
            get (fld, mvalue) =
                case mvalue of
                  Just val
                      → case AE.fromJSON val of
                          AE.Success res' → pure res'
                          AE.Error   e    → fail $ printf "while parsing field %s: %s, obj=%s" fld e (show val)
                  _   → fail $ missing "field" $ T.unpack fld
        (iid ∷ String)        ← get $ field "numberInProject"
        issue_summary         ← get $ field "summary"
        types                 ← mget $ field "Type"
        let issue_type        = head $ (fromMaybe [] types) <|> [Type "No Type"] -- XXX: per-project defaulting
        priority              ← mget $ field "Priority"
        let issue_priority    = head $ (fromMaybe [Priority "No Priority"] priority)
        author ∷ String       ← get $ field "reporterName"
        assignee ∷ Maybe String
                              ← mget $ field "Assignee"
        state                 ← mget $ field "State"
        let issue_state       = head $ (fromMaybe [] state) <|> [State "No State"] -- XXX: per-project defaulting
        PDate issue_created   ← get $ field "created"
        resolved              ← mget $ field "resolved"
        let issue_resolved    = fmap fromPDate resolved
        PDate issue_updated   ← get $ field "updated"
        issue_description     ← mget $ field "description"
        estimation ∷ Maybe [String]
                              ← mget $ field "Estimation"
        let issue_estimation  = fmap (Hours ∘ read) $ headMay $ fromMaybe [] estimation
        votes                 ← mget $ field "votes"
        let issue_votes       = fromMaybe 0 votes
        links                 ← mget $ field "links"
        let issue_links       = fromMaybe [] links
        issue_tags            ← o .: "tag"
        pure $ do
              Project{ project_alias, project_members } ← ask
              let issue_id       = IId $ printf "%s-%s" (fromPAlias project_alias) iid
                  issue_author   = lookup_member project_members $ MLogin author
                  issue_assignee = fmap (lookup_member project_members ∘ MLogin) assignee
                  handled_fields = ["numberInProject", "summary", "Type", "Priority", "reporterName"
                                   , "Assignee", "State", "created", "resolved", "updated", "description"
                                   , "Estimation", "votes", "links", "tag"]
                  issue_fields   = HM.filterWithKey (\k _ → not $ (∈) k handled_fields) fields
              pure Issue{..}
        -- let fields = constrFields $ head $ dataTypeConstrs (dataTypeOf ((⊥) ∷ IFields))

instance FromJSON    (Reader Project WorkItem) where { parseJSON
    = AE.withObject "WorkItem" $
      \o → do
        workitem_id             ← o .: "id"
        workitem_activity       ← o .: "worktype"
        Performed workitem_date ← o .: "date"
        author                  ← o .: "author"
        (duration ∷ Int)        ← o .: "duration"
        let workitem_duration   = Hours ∘ floor $ (fromIntegral duration / 60.0 ∷ Double)
        workitem_description    ← o .: "description"
        pure $ do
             Project{ project_members } ← ask
             let workitem_author = lookup_member project_members author
             pure WorkItem{..}; }


-- * Generic Exchange/RR (request/response) machinery
data ExchangeType = Get | Post

class JSONC (Response q) ⇒ Exchange q where
    data Request  q     ∷ *
    type Response q     ∷ *
    request_type        ∷ Request q → ExchangeType
    request_params      ∷ Request q → WR.Options → WR.Options
    request_type      _ = Get
    request_params    _ = id
    -- Mandatory method:
    request_urlpath     ∷ Request q → URLPath
    -- POST-only method:
    request_post_args   ∷ Request q → [WR.FormParam]
    request_post_args _ = []
instance Exchange q ⇒ Show (Request q) where
    show x = printf "#{Request %s / %s}" (show $ request_urlpath x) (show $ request_post_args x)
type family   JSONC c ∷ Constraint
type instance JSONC c = (Generic c, FromJSON c)
type family   ShowC c ∷ Constraint
type instance ShowC c = (Show c)

data EProjectAll     = EProjectAll
data EIssueByProject = EIssueByProject
data EIssue          = EIssue
data EIssueTTWItem   = EIssueTTWItem


-- * /project/all?{verbose}
--   https://confluence.jetbrains.com/display/YTD65/Get+Accessible+Projects
instance Exchange EProjectAll where
    data   Request  EProjectAll  = RProjectAll
    type   Response EProjectAll  = [Project]
    request_urlpath RProjectAll =
        URLPath "/project/all"
    request_params  RProjectAll params =
        params & WR.param "verbose" .~ ["true"]


-- * /issue/byproject/{project}?{filter}&{after}&{max}&{updatedAfter}&{wikifyDescription}
--   https://confluence.jetbrains.com/display/YTD65/Get+Issues+in+a+Project
--   Note, this seems a bit useless, because it doesn't allow to get custom fields
--   in one request.

-- instance Exchange EIssueByProject where
--     data   Request  EIssueByProject = RIssueByProject PAlias Filter deriving Show
--     type   Response EIssueByProject = [Issue]
--     request_urlpath (RIssueByProject (PAlias alias) query) =
--         URLPath $ "/issue/byproject/" <> alias
--     request_params  (RIssueByProject (PAlias alias) (Filter query)) params =
--         params & if not $ Data.List.null query
--                  then WR.param "query" .~ [T.pack query]
--                  else id


-- * /issue?{filter}&{with}&{max}&{after}
--   https://confluence.jetbrains.com/display/YTD65/Get+the+List+of+Issues
newtype RIssueWrapper = RIssueWrapper { issue ∷ [Reader Project Issue] } deriving (Generic)
instance FromJSON       RIssueWrapper where parseJSON = newtype_from_JSON
instance Exchange EIssue where
    data   Request  EIssue = RIssue Filter {-ignored-} Int {-ignored-} [Field]
    type   Response EIssue = [Reader Project Issue]
    request_urlpath (RIssue _ _ _) =
        URLPath $ "/issue"
    request_params  (RIssue (Filter query) limit fields) params =
        params & WR.param "filter" .~ [T.pack query]
               & WR.param "max"    .~ [T.pack $ printf "%d" limit]
               & WR.param "with"   .~ case fmap (T.pack ∘ field) fields of
                                        [] → [""]
                                        xs → xs


-- *  /issue/{issue}/timetracking/workitem/
--   https://confluence.jetbrains.com/display/YTD65/Get+Available+Work+Items+of+Issue
instance Exchange EIssueTTWItem where
    data   Request  EIssueTTWItem  = RIssueTTWItem IId
    type   Response EIssueTTWItem  = [Reader Project WorkItem]
    request_urlpath (RIssueTTWItem (IId iid)) =
        URLPath $ "/issue/" <> iid <> "/timetracking/workitem/"



-- * Request IO machinery
ytRequestRaw ∷ HASCALLSTACK (Exchange y) ⇒ YT → Request y → IO BL.ByteString
ytRequestRaw (YT (Access hostname ssl_opts _ _) wreq_opts jar) req = do
  let url    = "https://" <> hostname <> "/rest" <> (fromURLPath $ request_urlpath req)
      params = request_params req
      opts   = wreq_opts & (WR.header "Accept" .~ ["application/json"]) & params
  -- printf "--> %s %s\n" url (show $ opts ^. WR.params )
  r ← withOpenSSL $
      case request_type req of
        Get  → WR.getWith  opts url
        Post → WR.postWith opts url $ request_post_args req
  -- printf "<-- %s\n" $ show $ r ^. WR.responseBody
  pure $ r ^. WR.responseBody

ytDecode ∷ HASCALLSTACK (FromJSON r) ⇒ BL.ByteString → IO r
ytDecode bs = do
  case AE.eitherDecode bs of
    Left e     → error e
    Right resp → pure resp

ytRequest ∷ HASCALLSTACK (Exchange y, FromJSON (Response y)) ⇒ YT → Request y → IO (Response y)
ytRequest yt req =
    ytRequestRaw yt req >>=
    ytDecode

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
ytLoadRequest ∷ (Exchange y, FromJSON (Response y)) ⇒ Request y → IO (Response y)
ytLoadRequest = ytLoadJSON
