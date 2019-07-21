{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.HatenaBlog
    ( App(..)
    , HatenaConfig(..)
    , post
    , put
    ) where

import           Data.Aeson               (FromJSON)
import           Data.Aeson.TH            (defaultOptions, deriveFromJSON)
import           Data.Foldable            (asum)
import           GHC.Generics
import           Network.HTTP.Req
import           RIO
import           RIO.Text.Lazy            (toStrict)
import           System.FilePath          (takeBaseName)
import           Text.Blaze.Renderer.Text (renderMarkup)
import           Text.Heterocephalus
import qualified Text.XML                 as XML
import           Text.XML.Lens

data App = App
    { appLogFunc      :: !LogFunc
    , appHatenaConfig :: !HatenaConfig
    }

data HatenaConfig = HatenaConfig
    { hatenaId     :: !Text
    , hatenaBlogId :: !Text
    , hatenaAuth   :: !Text
    } deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''HatenaConfig)

instance HasLogFunc App where
    logFuncL = RIO.lens appLogFunc (\x y -> x { appLogFunc = y })

class HasConfig env where
    configL :: RIO.Lens' env HatenaConfig

instance HasConfig App where
    configL = RIO.lens appHatenaConfig (\x y -> x { appHatenaConfig = y })
instance HasConfig HatenaConfig where
    configL = id

post :: FilePath -> RIO App ()
post path = do
    App {..} <- ask
    let url = mkEntriesUrl appHatenaConfig
    let headers = mkHeader (hatenaAuth appHatenaConfig)
    body <- mkEntry path
    res  <- httpPost url headers body
    case parseResponseAsXml res of
        Right doc -> logInfo $ displayShow (getEntryUrl doc)
        Left  err -> logWarn $ displayShow err

put :: ByteString -> FilePath -> RIO App ()
put url path =
    case parseUrlHttps url of
        Just (url', _) -> updateEntry url' path
        Nothing        -> logWarn $ displayBytesUtf8 ("invalid url: " <> url)

updateEntry :: Url 'Https -> FilePath -> RIO App ()
updateEntry url path = do
    App {..} <- ask
    let headers = mkHeader (hatenaAuth appHatenaConfig)
    body <- mkEntry path
    res  <- httpPut url headers body
    case parseResponseAsXml res of
        Right doc -> logInfo $ displayShow (getEntryUrl doc)
        Left  err -> logWarn $ displayShow err

httpPost :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPost url headers body = runReq defaultHttpConfig (req POST url (ReqBodyBs body) lbsResponse headers)
httpPut  :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPut  url headers body = runReq defaultHttpConfig (req PUT url (ReqBodyBs body) lbsResponse headers)

parseResponseAsXml :: LbsResponse -> Either SomeException Document
parseResponseAsXml = XML.parseLBS XML.def . responseBody

mkEntry :: FilePath -> RIO env ByteString
mkEntry contentPath = do
    let title = takeBaseName contentPath
    content <- readFileUtf8 contentPath
    let entry = renderMarkup $(compileTextFile "./entry-template.xml")
    return . encodeUtf8 . toStrict $ entry

mkEntriesUrl :: HatenaConfig -> Url 'Https
mkEntriesUrl HatenaConfig{..} =
    https "blog.hatena.ne.jp" /: hatenaId /: hatenaBlogId /: "atom" /: "entry"

mkHeader :: Text -> Option 'Https
mkHeader auth = header "Authorization" ("Basic " <> encodeUtf8 auth)

getEntryUrl :: Document -> Maybe Text
getEntryUrl doc = asum hrefs
  where
    hrefs = doc ^.. root
        .  el "{http://www.w3.org/2005/Atom}entry"
        ./ el "{http://www.w3.org/2005/Atom}link" . attributeIs "rel" "edit" . attribute "href"
