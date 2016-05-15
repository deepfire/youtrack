{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

#if (__GLASGOW_HASKELL__ > 710)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module Youtrack.Types
    (
      YT(..), Credentials(..)

    -- * Data model
    , ProjectDict, lookup_project
    , Member(..),  lookup_member
    , Project(..), palias_iid_idstr
    , Issue(..),   issue_idstr, identify_issue, mandatoryIssueFields
    , Comment(..)
    , WorkItem(..)
    , Filter(..)
    , Field(..)
    , URLPath(..)
    , YTCmd(..)

    --
    , MLogin(..), MFullName (..)
    , PAlias(..), PName(..), PDate(..)
    , IId(..), Type(..), Priority(..), Summary(..), Description(..), State(..), Tag(..), Link(..)
    , Created(..), Resolved(..), Updated(..)
    , Performed(..), Activity(..)
    , Hours(..)

    --
    , newtype_from_JSON
    , youtrack_timeint_localtime
    , youtrack_datestring_localtime

    --
    , abbrev_object
    )
where

import           Control.Applicative         ((<|>))
import           Control.Monad.Reader
import           Control.Monad.Identity                
import           Data.Aeson                  (FromJSON (..), Value, (.:), (.:?))
import           Data.Aeson.Types            (Options (..))
import           Data.Hashable               (Hashable)
import           Data.List
import           Data.List.Split             (splitOn)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.String                 (IsString(..))
import           Data.Scientific             (Scientific)
import           Data.Time.LocalTime         (LocalTime (..), utcToLocalTime, hoursToTimeZone)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           GHC.Generics                (Generic(Rep))
import           Network.HTTP.Client         (CookieJar)
import           Prelude.Unicode
import           Safe                        (headMay)
import           Test.QuickCheck
import           Text.Printf                 (printf)

import qualified Data.Aeson                   as AE
import qualified Data.Aeson.Types             as AE
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Network.Wreq                 as WR

import           Youtrack.Names


-- * JSON stuff dump
value_map_lookup ∷ String → HM.HashMap T.Text AE.Value → T.Text → AE.Value
value_map_lookup desc hm key =
    HM.lookupDefault (error $ printf "missing %s: %s" desc $ show key) key hm

value_lookup ∷ String → T.Text → AE.Value → AE.Value
value_lookup desc key (AE.Object o) =
    value_map_lookup desc o key
value_lookup desc key val =
    error $ printf "while looking for key %s in %s: got non-object %s" (show key) desc $ show val

newtype_from_JSON, recdrop1_from_JSON ∷ (Generic a, AE.GFromJSON (Rep a)) ⇒ Value → AE.Parser a
newtype_from_JSON  = AE.genericParseJSON (AE.defaultOptions { unwrapUnaryRecords = True })
recdrop1_from_JSON = AE.genericParseJSON (AE.defaultOptions { fieldLabelModifier = drop 1 })


-- * Leaf types
newtype URLPath      = URLPath       { fromURLPath      ∷ String } deriving Show

newtype Hours        = Hours         { fromHours ∷ Int }           deriving (Eq, Generic, Ord, FromJSON)
newtype Filter       = Filter        { fromFilter ∷ String }       deriving (Generic)
newtype Field        = Field         { field ∷ String }            deriving (Generic)

newtype YTCmd        = YTCmd         { _fromYTCmd ∷ String }       deriving (Eq, Generic, Show)
instance IsString      YTCmd          where fromString = YTCmd

-- newtype PVersion     = PVersion   { fromPVersion ∷ String }     deriving (Generic)
-- instance FromJSON      PVersion                                 where parseJSON = newtype_from_JSON
-- newtype PVersionList = PVersionList [String]                    deriving (Generic)
-- instance FromJSON      PVersionList                             where
--     parseJSON (AE.Null)      = pure $ PVersionList []
--     parseJSON v@(AE.Array _) = AE.genericParseJSON AE.defaultOptions v
--     parseJSON v              = fail $ printf "unexpected value for a version list: %s" (show v)
youtrack_timeint_localtime ∷ Integer → LocalTime
youtrack_timeint_localtime = utcToLocalTime (hoursToTimeZone 4) ∘ posixSecondsToUTCTime ∘ fromIntegral ∘ (floor ∷ Double → Integer) ∘ ((/ 1000.0) ∷ Double → Double) ∘ fromIntegral
youtrack_datestring_localtime ∷ T.Text → LocalTime
youtrack_datestring_localtime = youtrack_timeint_localtime ∘ read ∘ T.unpack
newtype PDate        = PDate         { fromPDate ∷ LocalTime }     deriving (Eq, Generic, Ord)
instance FromJSON      PDate                                       where parseJSON = AE.withText "date" $ \n → do
                                                                                       pure ∘ PDate $ youtrack_datestring_localtime n
newtype Created      = Created       { fromCreated ∷ LocalTime }   deriving (Eq, Generic, Ord)
instance FromJSON      Created                                     where parseJSON = AE.withText "created date" $ \n → do
                                                                                       pure ∘ Created $ youtrack_datestring_localtime n
newtype IId          = IId           { fromIId ∷ Int }             deriving (Eq, Generic, Ord)
instance FromJSON      IId                                         where parseJSON = newtype_from_JSON
newtype Priority     = Priority      { fromPriority ∷ String }     deriving (Eq, Generic, Ord)
instance FromJSON      Priority                                    where parseJSON = newtype_from_JSON
newtype Resolved     = Resolved      { fromResolved ∷ LocalTime }  deriving (Eq, Generic, Ord)
instance FromJSON      Resolved                                    where parseJSON = AE.withText "resolved date" $ \n → do
                                                                                       pure ∘ Resolved $ youtrack_datestring_localtime n
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
                                                                                       pure ∘ Updated $ youtrack_datestring_localtime n
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
data Credentials where
     Credentials ∷
         { login        ∷ MLogin
         , password     ∷ Maybe String
         } → Credentials

data YT where
     YT ∷
         { ytHostname    ∷ String
         , ytRestSuffix  ∷ String
         , ytWreqOptions ∷ WR.Options
         , ytLogin       ∷ MLogin
         , ytJar         ∷ Maybe CookieJar
         } → YT

instance Arbitrary YT where
    arbitrary = YT <$> arbitrary <*> arbitrary
                <*> (error "Youtrack is not accessible: no Wreq options available.")
                <*> (pure $ MLogin "nobody") <*> pure Nothing

newtype PName        = PName         { fromPName ∷ String }        deriving (Generic)
newtype PAlias       = PAlias        { fromPAlias ∷ String }       deriving (Eq, Generic, Hashable, Read)
instance FromJSON      PName                                       where parseJSON = newtype_from_JSON
instance FromJSON      PAlias                                      where parseJSON = newtype_from_JSON

data Project =
    Project {
      project_yt            ∷ YT
    , project_alias         ∷ PAlias
    , project_name          ∷ PName
    , project_members       ∷ [Member] -- initially empty
    , project_accessor      ∷ Maybe Member
    } deriving (Generic)

data Member =
    Member {
      member_login          ∷ MLogin
    , member_fullname       ∷ MFullName
    } deriving (Generic)

palias_iid_idstr ∷ PAlias → IId → String
palias_iid_idstr PAlias{..} IId{..} = fromPAlias <> "-" <> show fromIId

issue_idstr ∷ Issue → String
issue_idstr Issue{..} = palias_iid_idstr (project_alias issue_project) issue_id

identify_issue ∷ PAlias → IId → Issue → Bool
identify_issue pa iid Issue{..} =
    iid ≡ issue_id ∧
    pa  ≡ project_alias issue_project


-- * FromJSON

instance FromJSON (ReaderT () Identity ()) where
    parseJSON _ = pure $ pure ()

instance FromJSON (Reader YT Project) where { parseJSON
    = AE.withObject "Project" $
      \o → do
        project_alias        ← o .: "shortName"
        project_name         ← o .: "name"
        logins ∷ [MLogin]    ← o .: "assigneesLogin"
        names  ∷ [MFullName] ← o .: "assigneesFullName"
        let project_members = fmap (\(l, n) → Member l n) $ zip logins names
        pure $ do
             project_yt@YT{..} ← ask
             let project_accessor = find (\Member{..} → ytLogin ≡ member_login) project_members
             pure Project{..}; }

lookup_member ∷ [Member] → MLogin → Member
lookup_member ms ((flip find) ms ∘ (\l m → l ≡ member_login m) → Just m) = m
lookup_member _ ml = error $ printf "Couldn't find project ∈ with login name '%s'." $ fromMLogin ml

lookup_project ∷ ProjectDict → PAlias → Project
lookup_project pd key@(PAlias keys) =
    flip fromMaybe (HM.lookup key pd) $
         error $ printf "Unknown project '%s'." keys

type ProjectDict = HM.HashMap PAlias Project


-- XXX: hard-coded assumptions about project issue structure
--      ..to be resolved by "2014 Bahr - Composing and Decomposing Data Types"
data Issue =
    Issue {
      issue_project         ∷ Project
    , issue_id              ∷ IId
    , issue_summary         ∷ Summary
    , issue_type            ∷ Type        --
    , issue_priority        ∷ Priority    --
    , issue_author          ∷ Member
    , issue_assignee        ∷ [Member] --
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

instance FromJSON ([Reader ProjectDict Issue]) where
    parseJSON = AE.withObject "Issue wrapper" $
       \o → case value_map_lookup "Issue list" o "issue" of
              AE.Array xs → forM (V.toList xs) parseJSON
              _erx        → error $ printf "Not an issue list in the 'value' element of issue list."

instance FromJSON (Reader ProjectDict Issue) where
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
        palias ∷ PAlias       ← get $ field "projectShortName"
        iid ∷ Int             ← fmap read ∘ get $ field "numberInProject"
        issue_summary         ← get $ field "summary"
        types                 ← mget $ field "Type"
        let issue_type        = head $ (fromMaybe [] types) <|> [Type "No Type"] -- XXX: per-project defaulting
        priority              ← mget $ field "Priority"
        let issue_priority    = head $ (fromMaybe [Priority "No Priority"] priority)
        author ∷ String       ← get $ field "reporterName"
        assignee ∷ Maybe [MLogin]
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
              projdict ← ask
              let issue_project@Project{ project_members } = lookup_project projdict palias
                  issue_id       = IId iid
                  issue_author   = lookup_member project_members $ MLogin author
                  issue_assignee = fmap (lookup_member project_members) $ fromMaybe [] assignee
                  handled_fields = ["numberInProject", "summary", "Type", "Priority", "reporterName"
                                   , "Assignee", "State", "created", "resolved", "updated", "description"
                                   , "Estimation", "votes", "links", "tag"]
                  issue_fields   = HM.filterWithKey (\k _ → not $ (∈) k handled_fields) fields
              pure Issue{..}
        -- let fields = constrFields $ head $ dataTypeConstrs (dataTypeOf ((⊥) ∷ IFields))

mandatoryIssueFields ∷ [String]
mandatoryIssueFields =
    [ "projectShortName", "numberInProject", "summary", "reporterName"
    , "created", "resolved", "updated", "description"
    , "votes", "links", "tag"]


data  Comment
    = Comment { comm_issue   ∷ Issue
              , comm_author  ∷ Member
              , comm_created ∷ LocalTime
              , comm_updated ∷ Maybe LocalTime
              , comm_text    ∷ String }

instance FromJSON (Reader Issue Comment)
    where parseJSON = AE.withObject "Comment" $ \o → do
            comm_text ∷ String ← fmap T.unpack $ o .: "text"
            comm_created ← fmap youtrack_timeint_localtime        $ o .:  "created"
            comm_updated ← fmap (fmap youtrack_timeint_localtime) $ o .:? "updated"
            author       ← o .: "author"
            pure $ do
              comm_issue@Issue{..} ← ask
              let comm_author = lookup_member (project_members issue_project) $ MLogin author
              pure Comment{..}


data WorkItem =
    WorkItem {
      workitem_id           ∷ String
    , workitem_activity     ∷ Maybe Activity
    , workitem_date         ∷ LocalTime
    , workitem_author       ∷ Member
    , workitem_duration     ∷ Hours
    , workitem_description  ∷ Maybe String
    } deriving (Generic)

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
