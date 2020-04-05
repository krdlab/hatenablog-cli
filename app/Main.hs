{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Yaml             (decodeFileThrow)
import qualified Options.Applicative   as Opts
import           RIO
-- import qualified RIO.ByteString  as BS
import           RIO.Text
import           System.Directory      (getHomeDirectory)
import           System.FilePath       ((</>))
import           Web.HatenaBlog        (App (..), HatenaConfig (..))
import qualified Web.HatenaBlog        as Blog


type BlogEntryURL = String

data Command
    = Post FilePath
    | Put BlogEntryURL FilePath
    | Get BlogEntryURL
    deriving Show

parsePost :: Opts.Parser Command
parsePost = Post <$> Opts.argument Opts.str (Opts.metavar "[PATH]")

parsePut :: Opts.Parser Command
parsePut = Put <$> Opts.argument Opts.str (Opts.metavar "[URL]")
               <*> Opts.argument Opts.str (Opts.metavar "[PATH]")

parseGet :: Opts.Parser Command
parseGet = Get <$> Opts.argument Opts.str (Opts.metavar "[URL]")

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc

parseSubcommands :: Opts.Parser Command
parseSubcommands = Opts.subparser subcommands
  where
    subcommands = Opts.command "post" (parsePost `withInfo` "Post blog entry")
               <> Opts.command "put" (parsePut `withInfo` "Update blog entry")
               <> Opts.command "get" (parseGet `withInfo` "Get blog entry")

parseCommand :: Opts.ParserInfo Command
parseCommand = parseSubcommands `withInfo` "Hatena Blog command line tool"

getHatenaConfig :: IO HatenaConfig
getHatenaConfig = do
    home <- getHomeDirectory
    decodeFileThrow (home </> ".hatena.yaml")

main :: IO ()
main = do
    opts <- Opts.execParser parseCommand
    conf <- getHatenaConfig

    let action = selectAction opts

    logOptions <- logOptionsHandle stdout False
    withLogFunc logOptions $ \logFunc -> do
        let app = App {
              appLogFunc = logFunc
            , appHatenaConfig = conf
            }
        runRIO app action

selectAction :: Command -> RIO App ()
selectAction c = case c of
    Post path    -> Blog.post path
    Put url path -> Blog.put (BS.pack url) path
    _            -> undefined

