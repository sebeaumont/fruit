{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Main program to process input documents, vectorize and send to ML backends
-- test bed for encoder and ML modules.

-- Contact: <mailto:simon.beaumont@clinitink.com>
-- Internal Use Only - All Rights Reserved.

-- TODO refactor by breaking monolith into tokenizer, framer and trainer parts

module Main where

import Control.Monad (unless, void)
import Control.Monad.State

import System.IO (isEOF, hPutStrLn, stderr)
import System.Console.CmdArgs

import Data.Aeson
import Data.Char

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B


-- | Record to represent a general document we might want to add an
-- optional corpus to this record...
data Document = Document {
  docid :: !String,
  content  :: !T.Text
  } deriving (Show)

-- | How to construct a Document from JSON
instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document
    <$> v .: "docid"
    <*> v .: "content"

-- | Cheap logger
logerr :: Show a => a -> IO ()
logerr = hPutStrLn stderr . show

-- | UserOptions/command line parameters
data UserOptions = UserOptions {
  indexTokens :: [T.Text],
  forward :: Int,
  behind :: Int
  } deriving (Show, Data, Typeable)


defaultToz :: UserOptions
defaultToz = UserOptions [] 7 7

-- main entry point
main :: IO ()
main = void $ do
  opts <- cmdArgs defaultToz
  print opts
  execStateT (proio opts) (tokenMap $ indexTokens opts) >>= print

-- | This map of tokens allow us to keep track of specific token occurrences
type TokenMap = Map.Map T.Text Int


-- | Make new token map from list of tokens
tokenMap :: [T.Text]  -> TokenMap
tokenMap l = Map.fromList [(t, 0) | t <- l]


------------------------------------
-- io effects with TokenMap state --
------------------------------------

-- | replacement io loop
-- call this to process a corpus of json lines via stdio
proio :: UserOptions -> StateT TokenMap IO ()
proio t = void $ do
  eof <- liftIO isEOF
  unless eof
    (liftIO B.getLine >>= (processDocumentM t . decodeDocument) >> proio t)


-- | process a set of tokens as a document also report any
-- decode errors to stdio (need line numbers back) 
processDocumentM :: UserOptions -> Either String [T.Text] -> StateT TokenMap IO ()
processDocumentM t r = void $
  case r of
    Left s -> liftIO $ logerr s
    Right ts -> framesM (forward t) $ tokenize ts

-- TODO rewrite this so as not to be doing i/o but return "frames" as list
-- of tokens with behind and forward from target token
-- see original python tokenizer...

-- | Frames of n from Int 
framesM :: Int -> [T.Text] -> StateT TokenMap IO ()
framesM _ [] = return ()
framesM n (t:ts) = frameM t (take n ts) >> framesM n ts

-- | Training pairs from frame (1 target with rest source)
frameM :: T.Text -> [T.Text] -> StateT TokenMap IO ()
frameM t = mapM_ (pairM t)

-- | Dump sample pairs to stdout
-- N.B. we could encodeUtf8 these to byte strings...
pairM :: T.Text -> T.Text -> StateT TokenMap IO (T.Text, T.Text)
pairM t s = do
  t' <- tagTokenM t
  s' <- tagTokenM s
  lift (putStrLn $ (T.unpack t') ++ "\t" ++ (T.unpack s') ++ "\t" ++ show (1.0 :: Double))
  return (t', s')

-- | Monadic wrapper for stateful token transactions
tagTokenM :: T.Text -> StateT TokenMap IO T.Text 
tagTokenM t =
  do tm <- get
     let (m, v) = tagToken tm t 
     put m
     return v


-- | Attempt to decode nth. line of data to a Document record returning documents
-- tokens or description of decode error
decodeDocument :: B.ByteString -> Either String [T.Text]
decodeDocument s =
  case eitherDecodeStrict' s :: Either String Document of
    Left e -> Left e
    Right d -> Right $ documentTokens d 

-- | Simple white space splitter turns a document into a list of tokens
documentTokens :: Document -> [T.Text]
documentTokens = T.words . content


-- | Text words need some cleanup
cleanToken :: T.Text -> T.Text
cleanToken = T.dropAround supressChars

supressChars :: Char -> Bool
supressChars c = isPunctuation c || isNumber c || isSymbol c  

rejectToken :: T.Text -> Bool
rejectToken t = T.null t || len t <2 || len t >20 || allSameChar t
  where
    len = T.length
    allSameChar :: T.Text -> Bool
    allSameChar s = T.all (== T.head s) s 
  
acceptToken :: T.Text -> Bool
acceptToken = not . rejectToken 

-- | Tranform tokens
tokenize :: [T.Text] -> [T.Text]
tokenize ts = filter acceptToken $ map cleanToken ts


-- | Append a unique tag to a token if it is in the given map
-- bump the counter in the map entry and return new map and token thus effected
tagToken :: TokenMap -> T.Text -> (TokenMap, T.Text)
tagToken tm t =
  let (v, m) = Map.updateLookupWithKey (\_ x -> Just (x + 1)) t tm in
    case v of
      Nothing -> (tm, t) 
      Just n -> (m, T.append t (tag n))

tag :: Int -> T.Text
tag n = T.pack $ "#" ++ show n


sampleText :: T.Text
sampleText = "PubMed:13211925        Various strains of influenza virus produce a cytopathogenic effect in cultures of HeLa cells. The virus could not be passed in series. Virus partially or even completely inactivated with respect to infectivity by exposure to 37 degrees C. or ultraviolet light retained some of its cytopathogenic effect. No evidence has been obtained of an increase in infectious virus in HeLa cultures, but an increase in hemagglutinins and in both viral and soluble complement-fixing antigens became detectable during incubation. These virus materials apparently were not released from these cells prior to their destruction. These results suggested that HeLa cells are capable of supporting an incomplete reproductive cycle of influenza virus. The fact that radioactive phosphorus was readily incorporated into the hemagglutinin supplies strong evidence for this interpretation."

