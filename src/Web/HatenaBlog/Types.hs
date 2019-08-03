module Web.HatenaBlog.Types
    ( Feed(..)
    , Entry(..)
    , toFeed
    , getEntry
    ) where

import           RIO
import           Text.XML.Lens

data Feed = Feed
    { feedFirst   :: !(Maybe Text)
    , feedLast    :: !(Maybe Text)
    , feedPrev    :: !(Maybe Text)
    , feedNext    :: !(Maybe Text)
    , feedEntries :: ![Entry]
    } deriving Show

data Entry = Entry
    { entryEdit  :: !Text
    , entryTitle :: !Text
    } deriving Show

toFeed :: Document -> Maybe Feed
toFeed doc = do
    es <- mapM toEntry entries
    return $ Feed {
          feedFirst = first
        , feedLast = last
        , feedPrev = prev
        , feedNext = next
        , feedEntries = es
    }
  where
    first = feedlink "first"
    last  = feedlink "last"
    prev  = feedlink "prev"
    next  = feedlink "next"

    feedlink :: Text -> Maybe Text
    feedlink rel = asum $
        doc ^.. root . el "{http://www.w3.org/2005/Atom}feed"
            ./ el "{http://www.w3.org/2005/Atom}link" . attributeIs "rel" rel . attribute "href"

    entries :: [Element]
    entries = doc ^.. root . el "{http://www.w3.org/2005/Atom}feed" ./ el "{http://www.w3.org/2005/Atom}entry"

toEntry :: Element -> Maybe Entry
toEntry e = do
    edit  <- e ^? el "{http://www.w3.org/2005/Atom}link" . attributeIs "rel" "edit" . attr "href"
    title <- e ^? el "{http://www.w3.org/2005/Atom}title" . text
    return $ Entry {
            entryEdit = edit
        , entryTitle = title
    }

getEntry :: Document -> Maybe Entry
getEntry doc = do
    e <- listToMaybe $ doc ^.. root . el "{http://www.w3.org/2005/Atom}entry"
    toEntry e
