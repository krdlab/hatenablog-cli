
module Web.HatenaBlog.Response
    ( decode
    , BlogEntryResponse(..)
    ) where

import           Data.Maybe       (Maybe, listToMaybe)
import           Network.HTTP.Req
import           RIO
import qualified Text.XML         as XML
import           Text.XML.Lens


data BlogEntryResponse = BlogEntryResponse
    { entryTitle   :: Text
    , entryEditURL :: Text
    }
    deriving Show

decode :: LbsResponse -> Either String BlogEntryResponse
decode lbs =
    case parseXml lbs of
        Left  err -> Left . show $ err
        Right doc -> fromDocument doc

parseXml :: LbsResponse -> Either SomeException Document
parseXml = XML.parseLBS XML.def . responseBody

fromDocument :: Document -> Either String BlogEntryResponse
fromDocument doc = do
    title <- case findTitle doc of
        Nothing -> Left "title not found"
        Just t  -> Right t
    url   <- case findEntryUrl doc of
        Nothing -> Left "entry edit url not found"
        Just u  -> Right u
    return $ BlogEntryResponse title url

findTitle :: Document -> Maybe Text
findTitle doc = listToMaybe texts
  where
    texts =
        doc ^.. root
            ./ el "{http://www.w3.org/2005/Atom}/entry"
                ./ el "{http://www.w3.org/2005/Atom}/title" ./ text

findEntryUrl :: Document -> Maybe Text
findEntryUrl doc = listToMaybe hrefs
  where
    hrefs =
        doc ^.. root
            ./ el "{http://www.w3.org/2005/Atom}entry"
                ./ el "{http://www.w3.org/2005/Atom}link" ./ attributeIs "rel" "edit" ./ attr "href"
