{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import qualified Data.ByteString.Char8 as B (pack)
import           Data.Yaml             (decodeFileThrow)
import           Options.Declarative
import           RIO
import           RIO.Text
import           System.Directory      (getHomeDirectory)
import           System.FilePath       ((</>))
import           Web.HatenaBlog        (App (..), HatenaConfig (..))
import qualified Web.HatenaBlog        as Blog


instance ArgRead ByteString

main :: IO ()
main = run_ $
    Group "Hatena Blog Command Line Tool"
    [ subCmd "post" postEntry
    , subCmd "put"  putEntry
    , subCmd "get"  getEntry
    , subCmd "list" listEntries
    ]

postEntry :: Arg "PATH" FilePath
          -> Cmd "Post a blog entry" ()
postEntry path = liftIO $ runAction action
  where
    action = Blog.post (get path)

putEntry :: Flag "" '["url"] "URL" "a blog entry URL" ByteString
         -> Arg "PATH" FilePath
         -> Cmd "Update a blog entry" ()
putEntry url path = liftIO $ runAction action
  where
    action = Blog.put (get url) (get path)

getEntry :: Flag "" '["url"] "URL" "a blog entry URL" ByteString
         -> Cmd "Get a blog entry" ()
getEntry = undefined

instance ArgRead Blog.Page where
    needArg _ = False

listEntries :: Flag "p" '["page"] "PAGE" "" (Maybe Blog.Page)
            -> Cmd "List blog entries" ()
listEntries page = liftIO $ runAction action
  where
    action = Blog.list (get page)

runAction :: RIO App () -> IO ()
runAction action = do
    hatenaConfig <- getHatenaConfig
    logOptions <- logOptionsHandle stdout False
    withLogFunc logOptions $ \logFunc -> do
        let app = App {
              appLogFunc = logFunc
            , appHatenaConfig = hatenaConfig
            }
        runRIO app action

getHatenaConfig :: IO HatenaConfig
getHatenaConfig = do
    home <- getHomeDirectory
    decodeFileThrow $ home </> ".hatena.yaml"
