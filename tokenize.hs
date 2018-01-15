{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Main program to process input documents, vectorize and send to ML backends
-- test bed for encoder and ML modules.
-- Contact: <mailto:simon.beaumont@clinitink.com>
-- Internal Use Only - All Rights Reserved.

module Main where

import Control.Monad (when)
import System.IO (isEOF)
import System.Console.CmdArgs
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString as B
--import Debug.Trace
import Database.SDM

-- | Command line options for this program
data SDRLearn = SDRLearn { database :: String
                         , size :: Integer
                         , maxsize :: Integer
                         } deriving (Show, Data, Typeable)

defaultSize :: Integer
defaultSize = 1024*1024*1024*4

defaultArgs :: SDRLearn
defaultArgs = SDRLearn { database = "embeds.sdr"
                       , size = defaultSize
                       , maxsize = defaultSize
                       }

-- | Record to represent a general document we might want to add an
-- optional corpus to this record... also try making fields strict
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
main = do
  args <- cmdArgs defaultArgs
  edb <- openDB (database args) (size args) (maxsize args)
  case edb of
    Left err -> error $ "can't open db: " ++ show err
    Right db -> iolines (trainf db) 0 >>= print

-- | Read and count lines from stdin dispatching data for Aeson to parse
-- bad JSON decodes are discarded and errors logged with line number.
iolines :: Trainer -> Int -> IO Int
iolines tf n = do
  ateof <- isEOF
  if ateof
    then return n
    else B.getLine >>= ((processDocument tf) . (decodeDocument n))
         >> iolines tf (n+1)


-- | Attempt to decode nth. line of data to a Document record returning an
-- error description or tokenizing document
decodeDocument :: Int -> B.ByteString -> Either String [T.Text]
decodeDocument n s =
  case eitherDecodeStrict' s :: Either String Document of
    Left e -> Left $  e ++ " at line: " ++ show (n+1)
    Right d -> Right [cleanToken t | t <- tokenizeDocument d]

-- | Simple white space splitter turns a document into a list of tokens
tokenizeDocument :: Document -> [T.Text]
tokenizeDocument = T.words . content

-- | Text words need some cleanup
cleanToken :: T.Text -> T.Text
cleanToken = T.dropAround isPunctuation  


-- | Report error or process with training action
processDocument :: Trainer -> Either String [T.Text] -> IO ()
processDocument tf r = case r of
  Left s -> putStrLn s
  Right ts -> trainframes tf ts 7


--
-- experimental training
--

type Trainer = (String -> String -> IO SDMStatus)

-- | Create a function for training target and source
-- by partially evaluating some arguments
trainf :: SDMDatabase -> Trainer
trainf db = superpose db "words" "words" 


-- | Training frames from document tokens 
trainframes :: Trainer -> [T.Text] -> Int -> IO ()
trainframes _ [] _ = return ()
trainframes tf (t:ts) n =
  frame tf t (take n ts) >> trainframes tf ts n 

-- | Training pairs from frame (1 target with rest source)
frame :: Trainer -> T.Text -> [T.Text] -> IO ()
frame f t = mapM_ (pair f t)

pair :: Trainer -> T.Text -> T.Text -> IO ()
pair f t s = do
  r <- f (T.unpack t) (T.unpack s)
  when (sdmError r) $ error $ "sdmError: " ++ show r


-- TODO with this sample buffer i.e. superpose all of frame
-- members with t and superpose t with all frame members.
-- should have good cache behaviour and arithmetic density

{- 
-- instrumented with naughty function...
isample :: T.Text -> Frame -> Double
isample t f = trace (show t ++ "\t" ++ show f) (sample t f)
-}
