{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (unless, void, forM_)

import System.IO (isEOF, hPutStrLn, stderr)
import System.Console.CmdArgs

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B

import Database.SQLite.Simple

import Data.Text.Analysis
import Document.JSON

-------------------------------------------------------
-- | command line options

data LoadGrams = LoadGrams { database :: String
                           , gramsfrom :: Int
                           , gramsto :: Int
                           } deriving (Show, Data, Typeable)

-- main program entry point

main :: IO ()
main = void $ do
  lgargs <- cmdArgs $ LoadGrams "test.db" 1 7
  conn <- open $ database lgargs
  putStrLn $ "Ensuring schema in: " ++ database lgargs
  ensureSchema conn
  putStrLn "Reading JSON documents from stdin..."
  ioio conn (gramsfrom lgargs) (gramsto lgargs)
  putStrLn "<EOF> ensure indexes..."
  ensureIndexes conn
  putStrLn "test query..."
  res <- getTermGrams conn "CT" 10 19  
  forM_ res (\a -> case a of (Only t) -> TIO.putStrLn t)
  putStrLn "done!"
  close conn


--  Cheap logger
logerr :: Show a => a -> IO ()
logerr = hPutStrLn stderr . show

--  IO loop read JOSN lines from stdin and try to decode to documents for processing
ioio :: Connection -> Int -> Int -> IO ()
ioio c m n = void $ do
  eof <- isEOF
  unless eof
    (B.getLine >>= (processDocument c m n . decodeDocument) >> ioio c m n)

-- just tell us what went wrong
type DecodeError = String

-- process each document 
processDocument :: Connection -> Int -> Int -> Either DecodeError Document -> IO ()
processDocument c m n r = case r of
  Left e -> logerr e
  Right d -> storeMNGrams c $ analyze m n d


-- process document content to list of list of NGrams
type Gram = [T.Text]
type NGrams = [Gram]


analyze :: Int -> Int -> Document -> [NGrams]
analyze m n d = mngrams (tokenize $ content d) m n

----------------------------
-- storing grams into rdb --
----------------------------

storeMNGrams :: Connection -> [NGrams] -> IO ()
storeMNGrams c ngs = mapM_ (storeGrams c) ngs

storeGrams :: Connection -> [Gram] -> IO ()
storeGrams c gs = mapM_ (storeGram c) gs

storeGram :: Connection -> Gram -> IO () 
storeGram c g = void $ do
  let glen = length g
  execute c insertGram $ Only glen
  gid <- lastInsertRowId c
  -- insert all the terms in the gram with the gram id and position
  executeMany c insertGramDetail (gramRows (fromIntegral gid) g glen)
  
gramRows :: Integer -> Gram -> Int -> [(Integer, T.Text, Int, Int)]   
gramRows i g l = zip4 (repeat i) g [1..] (repeat l)
  

-- query for phrases containing term, t at postion p and gram length l
getTermGrams :: Connection -> T.Text -> Int -> Int -> IO [Only(T.Text)]
getTermGrams c t p l = query c termGramQuery (t, p, l)

  
---------------------
-- gram rdb schema --
---------------------

-- denormalized the gram length into gram/term detail could normalize
-- terms into own table if space is an issue.
-- the idea is to optimize for query performance

ensureSchema :: Connection -> IO ()
ensureSchema c = do
  execute_ c ensureGramTable
  execute_ c ensureGramDetailTable


ensureGramTable :: Query
ensureGramTable = "CREATE TABLE IF NOT EXISTS gramid (id INTEGER PRIMARY KEY, length INTEGER)"

ensureGramDetailTable :: Query
ensureGramDetailTable = "CREATE TABLE IF NOT EXISTS grams (gramid INTEGER, term TEXT, position INTEGER, length INTEGER)" 

-- inserts

insertGram :: Query
insertGram = "INSERT INTO gramid (id, length) VALUES (NULL, ?)"

insertGramDetail :: Query
insertGramDetail = "INSERT INTO grams (gramid, term, position, length) VALUES (?,?,?,?)"

--  indexes after insertion 

ensureIndexes :: Connection -> IO ()
ensureIndexes c = do
  execute_ c indexGramid
  execute_ c indexTerm
  execute_ c indexLength
  execute_ c indexPosition

indexGramid :: Query
indexGramid = "CREATE INDEX IF NOT EXISTS grams_gramid ON grams(gramid)"

indexTerm :: Query
indexTerm = "CREATE INDEX IF NOT EXISTS grams_term ON grams(term)"

indexLength :: Query
indexLength = "CREATE INDEX IF NOT EXISTS grams_length ON grams(length)"

indexPosition :: Query
indexPosition = "CREATE INDEX IF NOT EXISTS grams_position ON grams(position)"


-- a query for grams as original text
-- not that useful for processing but handy for interactive queries/demo/debug

termGramQuery :: Query
termGramQuery = "SELECT group_concat(term, ' ') FROM grams WHERE gramid IN \
                 \ (SELECT distinct(gramid) FROM grams WHERE term=? AND position=? AND length=?) \
               \ GROUP BY gramid ORDER BY gramid, position"
