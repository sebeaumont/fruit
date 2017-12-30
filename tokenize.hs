{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (when)
import System.IO (isEOF)
import Data.Aeson
import Data.Text
import qualified Data.ByteString as B

-- | Record to represent a general document we might want to add an
-- optional corpus to this record...

data Document = Document {
  docid :: String,
  content  :: Text
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

-- | Attempt decode of data to Document record logging any errors

decodeDocument :: Int -> B.ByteString -> IO ()
decodeDocument n s =
  case eitherDecodeStrict' s :: Either String Document of
    Left e -> putStrLn $ e ++ " at line: " ++ show n
    Right d -> processDocument d

processDocument :: Document -> IO ()
processDocument = print

  
