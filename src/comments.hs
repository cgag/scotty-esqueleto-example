{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

import Data.Text (Text, append)
import qualified  Data.Text.Lazy as L
import Data.Monoid (mconcat, (<>))

import Control.Monad.IO.Class

import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, 
                            share, sqlSettings)

import Web.Scotty as S

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase| 
Comment
  body Text
  authorName Text
  thread ThreadId
  deriving Show

Thread
  slug Text
|]

{-TODO: monad for threads?-}
fillDB = runSqlite "sqltemp.sqlite" $ do
  runMigration migrateTables
  t <- insert $ Thread "thread/1/slug/"
  insert $ Comment "Hello"   "Curtis" t
  insert $ Comment "World"   "Justin" t
  insert $ Comment "Goodbye" "Curtis" t

getComments = runSqlite "sqltemp.sqlite" $ do
  comments <- select $ from $ \c -> return c
  return $ map (prettyComment . entityVal) comments

prettyComment c = commentAuthorName c <> ": " <> commentBody c

main :: IO ()
main = do
  fillDB
  (comment:_) <- getComments
  scotty 3000 $ do
    S.get "/:word" $ do
      html $ mconcat $ map L.fromStrict ["<h1>Scotty, ", comment, " me up!</h1>"] 
