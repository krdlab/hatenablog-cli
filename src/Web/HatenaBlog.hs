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
    , Page(..)
    , list
    ) where

import           Data.Aeson               (FromJSON)
import           Data.Aeson.TH            (defaultOptions, deriveFromJSON)
import           Data.Foldable            (asum)
import           Data.Maybe               (fromJust)
import           GHC.Generics
import           Network.HTTP.Req
import           RIO
import           RIO.Text.Lazy            (toStrict)
import           System.FilePath          (takeBaseName)
import           Text.Blaze.Renderer.Text (renderMarkup)
import           Text.Heterocephalus
import qualified Text.XML                 as XML
import           Text.XML.Lens
import           Web.HatenaBlog.Types

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
    config <- appHatenaConfig <$> ask
    let url = mkEntriesUrl config
    let header = authHeader config
    body <- mkEntry path
    res  <- httpPost url header body
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
    config <- appHatenaConfig <$> ask
    let header = authHeader config
    body <- mkEntry path
    res  <- httpPut url header body
    case parseResponseAsXml res of
        Right doc -> logInfo $ displayShow (getEntryUrl doc)
        Left  err -> logWarn $ displayShow err

newtype Page = Page
    { unPage :: String
    } deriving (Eq, Show, Read)

list :: Maybe Page -> RIO App ()
list page = do
    config <- appHatenaConfig <$> ask
    let url = mkEntriesUrl config
    let header = authHeader config
    let options = header <> paramPage page
    res <- httpGet url options
    case parseResponseAsXml res of
        Right doc -> logInfo $ displayShow (toFeed doc)
        Left  err -> logWarn $ displayShow err
  where
    paramPage (Just (Page p)) = "page" =: p
    paramPage Nothing         = mempty

httpPost :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPost url options body = runReq defaultHttpConfig (req POST url (ReqBodyBs body) lbsResponse options)
httpPut  :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPut  url options body = runReq defaultHttpConfig (req PUT url (ReqBodyBs body) lbsResponse options)
httpGet  :: MonadIO m => Url 'Https -> Option 'Https -> m LbsResponse
httpGet  url options = runReq defaultHttpConfig (req GET url NoReqBody lbsResponse options)

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

authHeader :: HatenaConfig -> Option 'Https
authHeader HatenaConfig{..} = header "Authorization" ("Basic " <> encodeUtf8 hatenaAuth) -- FIXME: basicAuth

getEntryUrl :: Document -> Maybe Text
getEntryUrl doc = entryEdit <$> getEntry doc
