{-# LANGUAGE OverloadedStrings #-}

-- | A module of text analysis utilities where textural data my be
-- broken into a stream of tokens for the purpose of further analysis,
-- e.g. language models and NLP.  We also re-export some useful
-- modules so as to be a "one stop shop" for pre-processing text.

module Data.Text.Analysis ( tokenize
                          , mngrams
                          , ngrams
                          , allgrams
                          , TokenMap
                          , tokenMap
                          , tagToken
                          , module Data.Text.Metrics
                          ) where 


import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Text.Metrics -- re-exported as is

-- | Transform a Data.Text into a list of tokens by splitting and cleaning.
tokenize :: T.Text -> [T.Text]
tokenize = filter acceptToken . (map cleanToken) . T.words

-- Text words need some cleanup
-- TODO make the cleanup classes parameters of the tokenize function

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


--------------------------------------------------------------------
-- ngrams
-- since these operate on lists in general perhaps they
-- should be in their own module.

-- | grams of length m to n
mngrams :: [a] -> Int -> Int -> [[[a]]]
mngrams l m n = map (ngrams l) [m..n]

-- | all grams (of length 1 to n)
allgrams :: [a] -> Int -> [[[a]]]
allgrams l = mngrams l 1 

-- | grams of length n
ngrams :: [a] -> Int -> [[a]]
ngrams l n = take (length l - (n - 1)) . map (take n) . tails $ l


-----------------------------------------------
-- under consideration for tagging tokenizer...

-- | This map of tokens allow us to keep track of specific token occurrences
type TokenMap = Map.Map T.Text Int


-- | Make new token map from list of tokens
tokenMap :: [T.Text]  -> TokenMap
tokenMap l = Map.fromList [(t, 0) | t <- l]


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

{-
sampleText :: T.Text
sampleText = "PubMed:13211925        Various strains of influenza virus produce a cytopathogenic effect in cultures of HeLa cells. The virus could not be passed in series. Virus partially or even completely inactivated with respect to infectivity by exposure to 37 degrees C. or ultraviolet light retained some of its cytopathogenic effect. No evidence has been obtained of an increase in infectious virus in HeLa cultures, but an increase in hemagglutinins and in both viral and soluble complement-fixing antigens became detectable during incubation. These virus materials apparently were not released from these cells prior to their destruction. These results suggested that HeLa cells are capable of supporting an incomplete reproductive cycle of influenza virus. The fact that radioactive phosphorus was readily incorporated into the hemagglutinin supplies strong evidence for this interpretation."
-}

