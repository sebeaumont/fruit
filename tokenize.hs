{-# LANGUAGE OverloadedStrings #-}

-- | Main program to process input documents, vectorize and send to ML backends
-- test bed for encoder and ML modules.
-- Contact: <mailto:simon.beaumont@clinitink.com>
-- Internal Use Only - All Rights Reserved.

module Main where

import System.IO (isEOF)
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString as B
--import qualified Data.Sequence as S
--import Debug.Trace
import Database.SDM


-- | Record to represent a general document we might want to add an
-- optional corpus to this record...
data Document = Document {
  docid :: String,
  content  :: T.Text
  } deriving (Show)

instance FromJSON Document where
  parseJSON (Object v) =
    Document
    <$> v .: "docid"
    <*> v .: "content"

-- main entry point
main :: IO ()
main = iolines 0 >>= print

-- | Read and count lines from stdin dispatching data for Aeson to parse
-- bad JSON decodes are discarded and errors logged with line number.
iolines :: Int -> IO Int
iolines n = do
  ateof <- isEOF
  if ateof
    then return n
    else B.getLine >>= decodeDocument n
         >> iolines (n+1)


-- | Attempt to decode nth. line of data to a Document record logging any
-- errors or processing document.
decodeDocument :: Int -> B.ByteString -> IO ()
decodeDocument n s =
  case eitherDecodeStrict' s :: Either String Document of
    Left e -> putStrLn $ e ++ " at line: " ++ show (n+1)
    Right d -> processDocument d

-- | test output driver
processDocument :: Document -> IO ()
processDocument d = 
    print $ samples [cleanToken t | t <- tokenizeDocument d] 7


-- | Simple white space splitter turns a document into a list of tokens
tokenizeDocument :: Document -> [T.Text]
tokenizeDocument = T.words . content

-- | Text words need some cleanup
cleanToken :: T.Text -> T.Text
cleanToken = T.dropAround isPunctuation  


--
-- experimental training
--

type Frame = [T.Text] 

-- TODO for external training we need to pass a training action through to
-- sample.. or use processDocument as this is driver and keep these pure

-- | samples get list of tokens in a doc and window size
samples :: [T.Text] -> Int -> Double  
samples [] _ = 0.0
samples (t:ts) n  = sample t (take n ts) + samples ts n


{-
-- instrumented with naughty function...
isample :: T.Text -> Frame -> Double
isample t f = trace (show t ++ "\t" ++ show f) (sample t f)
-}

-- | TODO with this sample buffer i.e. superpose all of frame
-- members with t and superpose t with all frame members.
-- should have good cache behaviour and arithmetic density

sample :: T.Text -> Frame -> Double
sample t f = 1.0 -- we could return some property of the sample vector here
    
