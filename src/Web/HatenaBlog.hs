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
import           Web.HatenaBlog.Response

data App = App
    { appLogFunc      :: !LogFunc
    , appHatenaConfig :: !HatenaConfig
    }

instance HasLogFunc App where
    logFuncL = RIO.lens appLogFunc (\x y -> x { appLogFunc = y })

data HatenaConfig = HatenaConfig
    { hatenaId     :: !Text
    , hatenaBlogId :: !Text
    , hatenaAuth   :: !Text
    } deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''HatenaConfig)

class HasConfig env where
    configL :: RIO.Lens' env HatenaConfig
instance HasConfig App where
    configL = RIO.lens appHatenaConfig (\app c -> app { appHatenaConfig = c })
instance HasConfig HatenaConfig where
    configL = id

class (HasLogFunc env, HasConfig env) => HasEnv env where
    envL :: RIO.Lens' env App
instance HasEnv App where
    envL = id

post :: HasEnv env => FilePath -> RIO env ()
post path = do
    url <- createPostEntryURL
    headers <- createHeaders
    body <- createEntry path
    res  <- httpPost url headers body
    case decode res of
        Right entry -> logInfo $ displayShow (entryEditURL entry)
        Left  err   -> logWarn $ displayShow err

put :: HasEnv env => ByteString -> FilePath -> RIO env ()
put url path =
    case parseUrlHttps url of
        Just (url', _) -> updateEntry url' path
        Nothing        -> logWarn $ displayBytesUtf8 ("invalid url: " <> url)

updateEntry :: HasEnv env => Url 'Https -> FilePath -> RIO env ()
updateEntry url path = do
    headers <- createHeaders
    body <- createEntry path
    res  <- httpPut url headers body
    case decode res of
        Right entry -> logInfo $ displayShow (entryEditURL entry)
        Left  err   -> logWarn $ displayShow err

createHeaders :: HasEnv env => RIO env (Option 'Https)
createHeaders = do
    HatenaConfig{..} <- RIO.view configL
    let auth = encodeUtf8 hatenaAuth
    return $ header "Authorization" ("Basic " <> auth)

httpPost :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPost url headers body = runReq defaultHttpConfig (req POST url (ReqBodyBs body) lbsResponse headers)
httpPut  :: MonadIO m => Url 'Https -> Option 'Https -> ByteString -> m LbsResponse
httpPut  url headers body = runReq defaultHttpConfig (req PUT url (ReqBodyBs body) lbsResponse headers)

createEntry :: HasEnv env => FilePath -> RIO env ByteString
createEntry contentPath = do
    let title = takeBaseName contentPath
    content <- readFileUtf8 contentPath
    let entry = renderMarkup $(compileTextFile "./entry-template.xml")
    return . encodeUtf8 . toStrict $ entry

createPostEntryURL :: HasEnv env => RIO env (Url 'Https)
createPostEntryURL = do
    HatenaConfig{..} <- RIO.view configL
    return $ https "blog.hatena.ne.jp" /: hatenaId /: hatenaBlogId /: "atom" /: "entry"
