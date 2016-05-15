{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Authinfo
    ( readAuthInfo
    , readAuthInfoFile
    , getPassword )
where

import           Control.Applicative         ((<|>))
import           Control.Monad               (join)
import           Data.List                   (find)
import           Data.Monoid                 ((<>))
import           Prelude.Unicode
import qualified System.Environment
import qualified Text.Parser.Char             as P
import qualified Text.Parser.Combinators      as P
import qualified Text.Parser.Token            as P
import qualified Text.Trifecta.Parser         as P


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
