{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

#if (__GLASGOW_HASKELL__ > 710)
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Youtrack.Exchanges
    (
      Exchange(..)
    , ExchangeType(..)
    , Request(..)
    , Response

    , EProjectAll
    , EIssue
    , EIssueTTWItem
    , EIssueExecute
    , EIssueUpdate
    , EIssueComments
    )
where

import           Control.Lens         hiding (from, Context)
import           Control.Monad.Reader
import           GHC.Exts                    (Constraint)
import           GHC.Generics                (Generic)
import           Data.Aeson                  (FromJSON (..))
import           Data.Monoid                 ((<>))
import qualified Data.Text                    as T
import           Network.Wreq                (FormParam((:=)))
import qualified Network.Wreq                 as WR
import           Prelude.Unicode
import           Text.Printf                 (printf)

import           Youtrack.Types


-- * Generic Exchange/RR (request/response) machinery
type family   JSONC c ∷ Constraint
type instance JSONC c = (FromJSON c)
type family   FunctorC (c ∷ * → *) ∷ Constraint
type instance FunctorC c = (Functor c)
type family   ShowC c ∷ Constraint
type instance ShowC c = (Show c)

data ExchangeType = Get | Post

type PreResponse q = Replies q (Reader (Context q) (Reply q))
type    Response q = Replies q                     (Reply q)

class (Functor (Replies q), JSONC (PreResponse q)) ⇒ Exchange q where
    type Context  q     ∷ *
    data Request  q     ∷ *
    type Replies  q     ∷ * → *
    type Reply    q     ∷ *
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


-- * /project/all?{verbose}
--   https://confluence.jetbrains.com/display/YTD65/Get+Accessible+Projects
data                EProjectAll
instance Exchange   EProjectAll where
    type   Context  EProjectAll = YT
    data   Request  EProjectAll = RProjectAll
    type   Replies  EProjectAll = []
    type   Reply    EProjectAll = Project
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
newtype RIssueWrapper = RIssueWrapper [Reader ProjectDict Issue] deriving (Generic)
instance FromJSON       RIssueWrapper where parseJSON = newtype_from_JSON
data                EIssue
instance Exchange   EIssue where
    type   Context  EIssue = ProjectDict
    data   Request  EIssue = RIssue Filter {-ignored-} Int {-ignored-} [Field]
    type   Replies  EIssue = []
    type   Reply    EIssue = Issue
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
data                EIssueTTWItem
instance Exchange   EIssueTTWItem where
    type   Context  EIssueTTWItem  = Project
    data   Request  EIssueTTWItem  = RIssueTTWItem PAlias IId
    type   Replies  EIssueTTWItem  = []
    type   Reply    EIssueTTWItem  = WorkItem
    request_urlpath (RIssueTTWItem palias iid) =
        URLPath $ "/issue/" <> palias_iid_idstr palias iid <> "/timetracking/workitem/"


-- * POST /issue/{issue}/execute?{command}&{comment}&{group}&{disableNotifications}&{runAs}
--   https://confluence.jetbrains.com/display/YTD65/Apply+Command+to+an+Issue
data              EIssueExecute
instance Exchange EIssueExecute where
    type Context  EIssueExecute    = ()
    data Request  EIssueExecute    = RIssueExecute PAlias IId YTCmd Bool | RIssuePostComment PAlias IId String
    type Replies  EIssueExecute    = Identity
    type Reply    EIssueExecute    = ()
    request_type                 _ = Post
    request_urlpath r = let p_i = case r of RIssueExecute     pal' iid' _ _ → (pal', iid')
                                            RIssuePostComment pal' iid' _   → (pal', iid')
                        in URLPath $ "/issue/" <> palias_iid_idstr (fst p_i) (snd p_i) <> "/execute"
    request_post_args (RIssueExecute _ _ (YTCmd cmd) quiet) = [ "command" := T.pack cmd, "disableNotifications" := (T.pack $ if quiet then "true" else "false") ]
    request_post_args (RIssuePostComment _ _ comment)       = [ "comment" := T.pack comment ]


-- * POST /issue/{issueID}?{summary}&{description}
--   https://confluence.jetbrains.com/display/YTD65/Update+an+Issue
data              EIssueUpdate
instance Exchange EIssueUpdate where
    type Context  EIssueUpdate     = ()
    data Request  EIssueUpdate     = RIssueUpdate PAlias IId Summary Description
    type Replies  EIssueUpdate     = Identity
    type Reply    EIssueUpdate     = ()
    request_type                 _ = Post
    request_urlpath (RIssueUpdate pal iid _ _) =
        URLPath $ "/issue/" <> palias_iid_idstr pal iid
    request_post_args (RIssueUpdate _ _ summ desc) = [ "summary"     := (T.pack $ fromSummary     summ)
                                                     , "description" := (T.pack $ fromDescription desc) ]


-- * GET /rest/issue/{issue}/comment&{wikifyDescription}
--   https://confluence.jetbrains.com/display/YTD65/Get+Comments+of+an+Issue
data              EIssueComments
instance Exchange EIssueComments where
    type Context  EIssueComments   = Issue
    data Request  EIssueComments   = RIssueComments PAlias IId
    type Replies  EIssueComments   = []
    type Reply    EIssueComments   = Comment
    request_urlpath (RIssueComments pal iid) =
        URLPath $ "/issue/" <> palias_iid_idstr pal iid  <> "/comment"
