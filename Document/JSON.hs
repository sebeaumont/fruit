{-# LANGUAGE OverloadedStrings #-}

-- | This module allows bytestrings to be decoded from JSON as `Document`s
module Document.JSON (Document, decodeDocument, docid, content) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as B


-- | Record to represent a general document we might want to add an
-- optional corpus to this record...
data Document = Document {
  docid :: !String,
  content  :: !T.Text
  } deriving (Show)

-- | Construct a Document from JSON
instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document
    <$> v .: "docid"
    <*> v .: "content"

-- | Attempt to decode `ByteString` to a `Document` record
decodeDocument :: B.ByteString -> Either String Document
decodeDocument = eitherDecodeStrict'





