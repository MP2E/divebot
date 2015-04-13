{-# LANGUAGE TemplateHaskell, OverloadedLists, OverloadedStrings #-}
import Pipes
import Data.Text
import Data.Aeson
import Data.Maybe
import System.Exit
import Data.Text.IO
import Data.Aeson.TH
import Control.Monad
import Text.Show.Text
import Network.Socket
import System.FilePath
import Data.Time.Clock
import System.Directory
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Lens hiding ((.:),(.=))
import System.IO    hiding (hPutStrLn, readFile, print)
import Prelude      hiding (show, readFile, print, drop, toLower, putStrLn)

import qualified Data.Set             as S
import qualified Data.Map             as M
import qualified Data.Vector          as V
import qualified Pipes.Prelude        as P
import qualified Data.Text.Format     as F
import qualified Data.ByteString.Lazy as B
import qualified Prelude              as PR
import qualified Data.Char            as C (toLower)

$(deriveJSON defaultOptions ''PortNumber)

data Server = Server
    { _chatnet  :: Text
    , _address  :: Text
    , _port     :: PortNumber
    , _channels :: V.Vector Text
    , _nick     :: Maybe Text }
makeLenses ''Server

$(deriveJSON defaultOptions{fieldLabelModifier = PR.drop 1} ''Server)

data Config = Config
    { _globalNick    :: Text
    , _globalServers :: V.Vector Server }
makeLenses ''Config

$(deriveJSON defaultOptions{fieldLabelModifier = (fmap C.toLower) . PR.drop 7} ''Config)

data Bot = Bot { _handle    :: V.Vector Handle
               , _starttime :: UTCTime
               , _config    :: Config }
makeLenses ''Bot

data ChatMap = ChatMap { _markov  :: M.Map (V.Vector Text) (S.Set Text)
                       , _entryDb :: S.Set Text }
makeLenses ''ChatMap

-- The 'Net' type, a wrapper over Reader and IO.
type Net = ReaderT Bot IO

main :: IO ()
main = do
    c <- cfgHandler
    let mConfig = (decode c :: Maybe Config)
    when (isNothing mConfig) $ do
        hPutStrLn stderr "error: Could not decode config, check for syntax errors or missing fields"
        exitFailure
    let config = fromJust mConfig
    return () -- remove
--  bracket ircConnect ircDisconnect loop
--where
--  disconnect s = V.sequence_ . fmap hClose $ view handle s

{-|
ircConnect :: IO Bot
ircConnect = notify $ do
    t <- getCurrentTime
    h <- connectTo server port -- psuedocode
    hSetBuffering h NoBuffering
    hSetBuffering stdout LineBuffering -- Needs to be explicit on Windows!
    hSetEncoding stdout utf8
    return (Bot h t)
  where
    notify = bracket_
        (F.print "Connecting to {} ... " (F.Only server) >> hFlush stdout)
        (putStrLn "done.")
|-}

-- cfgHandler establishes the config directory and checks to see if divebot.cfg exists in it, if not, error out. If so, return the contents of the cfg file
cfgHandler :: IO B.ByteString
cfgHandler = do
    a         <- try getHomeDirectory :: IO (Either IOError FilePath)
    configDir <- either (\_ -> hPutStrLn stderr "warning: Couldn't find HOME directory, using current working directory instead" >> getCurrentDirectory) (\y -> return (y </> ".config")) a
    c         <- doesDirectoryExist configDir
    unless c $ createDirectory configDir
    let config = configDir </> "divebot.cfg"
    d         <- doesFileExist config
    unless d $ do
        hPutStrLn stderr $ "error: Couldn't find divebot.cfg at " `append` show configDir `append` ". Please create a config based off of example/divebot.cfg"
        exitFailure
    B.readFile config
