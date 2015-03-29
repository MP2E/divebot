{-# LANGUAGE DeriveGeneric, TemplateHaskell, OverloadedStrings #-}
import Network
import System.IO
import System.Exit
import GHC.Generics
import Control.Lens
import System.Random
import Text.Show.Text
import Data.Time.Clock
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Reader
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Map.Strict ((!))
import Control.Arrow (first)
import Data.Serialize.Text ()
import Control.Monad (unless, when)
import Data.Serialize (encode, decode)
import Data.List hiding ((++))
import Prelude   hiding ((++), show)

import qualified Data.Set         as S
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import qualified Data.Serialize   as C
import qualified Data.ByteString  as BS
import qualified Data.Map.Strict  as Map
import qualified Data.Text.Format as F

(++) :: T.Text -> T.Text -> T.Text
(++) = T.append

server = "irc.oftc.net"
port   = 6667
chan   = "#mp2e-testing"
nick   = "divebot"
brain  = "markov_brain.brn"

data Bot = Bot { _socket :: Handle, _starttime :: UTCTime }
makeLenses ''Bot

data ChatMap = ChatMap { _markov :: Map.Map [T.Text] (S.Set T.Text), _entryDb :: S.Set T.Text } deriving Generic
instance C.Serialize ChatMap
makeLenses ''ChatMap

-- The 'Net' type, a wrapper over Reader, State, and IO.
type Net = ReaderT Bot (StateT ChatMap IO)

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . view socket
    loop r     = evalStateT (runReaderT run r) ChatMap { _markov = Map.empty, _entryDb = S.empty }

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getCurrentTime
    h <- connectTo (T.unpack server) (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetBuffering stdout LineBuffering -- Needs to be explicit on Windows!
    hSetEncoding stdout utf8
    return (Bot h t)
  where
    notify = bracket_
        (F.print "Connecting to {} ... " (F.Only server) >> hFlush stdout)
        (T.putStrLn "done.")

-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :MP2E's bot") -- if modifying the user description, only modify after the :
    write "JOIN" chan
    asks (view socket) >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- T.init `fmap` io (T.hGetLine h)
    io (T.putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    ping x        = "PING :" `T.isPrefixOf` x
    pong x        = write "PONG" (T.cons ':' $ T.drop 6 x)
    clean         = T.drop 1 . T.unwords . drop 3 . T.words . cleanStatus
    cleanStatus x = if cleanPred x then T.empty else x -- remove anything that satisfies cleanPred
    cleanPred x   = ((T.drop 3 server `T.isInfixOf` x) &&
                    not (("user" ++ T.drop 3 server) `T.isInfixOf` x)) ||
                    ((nick ++ "!~" ++ nick) `T.isInfixOf` x) ||
                    ("JOIN :" `T.isInfixOf` x)  || ("PART :" `T.isInfixOf` x) ||
                    ("QUIT :" `T.isInfixOf` x)  || ("MODE" `T.isInfixOf` x) ||
                    ("NICK :" `T.isInfixOf` x)

-- Dispatch a command
eval :: T.Text -> Net ()
eval     "!quit"                        = write "QUIT" ":Exiting" >> io exitSuccess
eval     "!uptime"                      = uptime >>= privmsg
eval     "!loadstate"                   = readBrain            -- read from file and deserialize markov chain
eval     "!savestate"                   = writeBrain           -- serialize markov chain and write to file
eval     "!parsefile"                   = privmsg "error: enter servername/#channel.log to parse"
eval x | T.null x                       = return ()            -- ignore, empty list indicates a status line
eval x | "!parsefile " `T.isPrefixOf` x = parseChatLog x       -- parse an irssi chatlog to create an initial markov state
eval x | "!id " `T.isPrefixOf` x        = privmsg (T.drop 4 x)
eval x                                  = do
    let xs          = (sanitize . removeLinks . T.words) x
        highlight   = (nick ++ ": ") `T.isPrefixOf` x
        sanitize ys = if highlight then drop 1 ys else ys
    i <- io $ getStdRandom $ randomR (0,99) :: Net Int
    when ((i<4) || highlight) markovSpeak
    updateEntryDb xs
    createMarkov xs

removeLinks :: [T.Text] -> [T.Text]
removeLinks = filter $ \x -> not $ ("http://" `T.isPrefixOf` x) || ("https://" `T.isPrefixOf` x)

updateEntryDb :: [T.Text] -> Net ()
updateEntryDb [] = return () -- parseChatLog passes [] if it parses a status line
updateEntryDb xs = modify $ over entryDb $ S.insert (head xs)

parseChatLog :: T.Text -> Net ()
parseChatLog x  = do
    let statusPred x = ("---" `T.isPrefixOf` x)    || ("-!-" `T.isInfixOf` x) -- remove lines matching these predicates
        clean x = if statusPred x then [] else drop 3 $ T.words x
        f = T.drop 11 x
    res <- io $ try (fmap T.lines $ T.readFile $ T.unpack ("/home/cray/irclogs/" ++ f))
    case res of
         Left  e      -> io . T.hPutStrLn stderr $ "Warning: Couldn't open " ++ f ++ ": " ++ show (e :: IOError)
         Right rawlog -> do sequence_ $ fmap (\y -> let ys = (removeLinks . clean) y in updateEntryDb ys >> createMarkov ys) rawlog
                            privmsg "successfully parsed!"

writeBrain :: Net ()
writeBrain = do
    c          <- get
    io $ BS.writeFile brain $ encode c

readBrain :: Net ()
readBrain = do
    res  <- io . try $ BS.readFile brain
    case res of
         Left  e -> io $ T.hPutStrLn stderr $ "Warning: Couldn't open " ++ T.pack brain ++ ": " ++ show (e :: IOError)
         Right contents -> either (io. putStrLn) put $! decode contents

-- wrapper around markov sentence generation
markovSpeak :: Net ()
markovSpeak = do
    c        <- get
    start    <- io . searchMap (view entryDb c) $ Map.keys (view markov c)
    sentence <- io $ assembleSentence c start
    unless (T.null sentence) $ privmsg sentence

-- grabs random entry-point from entryDb and returns all possible keys
searchMap :: S.Set T.Text -> [[T.Text]] -> IO [[T.Text]]
searchMap xs k | S.null xs = return []
searchMap xs k             = do
    let m = length (S.toList xs) - 1
    startPoint <- getStdRandom $ randomR (0,m)
    return $ filter (((startPoint `S.elemAt` xs) ==) . head) k

-- assembles the sentence, taking random paths if the sentence branches
assembleSentence :: ChatMap -> [[T.Text]] -> IO T.Text
assembleSentence c [] = return T.empty
assembleSentence c xs = do
    let m = length xs - 1
    startPoint <- getStdRandom $ randomR (0,m)
    fmap T.unwords $ subAssembler $ xs !! startPoint
  where
    subAssembler :: [T.Text] -> IO [T.Text]
    subAssembler []     = return []
    subAssembler [y]    = return [y]
    subAssembler (y:ys) = fmap (y:) $ do
        let y2     = head ys
            values = Map.lookup [y, y2] $ view markov c
            m2     = case values of
                         Nothing -> -1
                         Just z  -> length z - 1
        if m2 < 0
        then return [y2]
        else do
            point    <- getStdRandom $ randomR (0,m2)
            subAssembler [y2, point `S.elemAt` fromJust values]

-- create the markov chain and store it in our ChatMap
createMarkov :: [T.Text] -> Net ()
createMarkov []     = return () -- parseChatLog passes [] if it parses a status line
createMarkov [x]    = modify $ over markov (Map.insert [x] S.empty)
createMarkov (x:xs) = do
    modify $ over markov (Map.insertWith S.union keys $ maybe S.empty S.singleton value)
    createMarkov xs
  where keys              = [x, head xs]
        value             = xs `safeIndex` 1

-- safe version of (!!)
safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _         = Nothing
safeIndex _      n | n < 0 = Nothing
safeIndex (x:_)  0         = Just x
safeIndex (_:xs) n         = safeIndex xs (n-1)

uptime :: Net T.Text
uptime = do
    now  <- io getCurrentTime
    zero <- asks $ view starttime
    return . pretty $ diffUTCTime now zero

-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: NominalDiffTime -> T.Text
pretty td =
  T.unwords $ fmap (uncurry (++) . first (show :: Int -> T.Text)) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (floor td,[]) metrics

-- Send a privmsg to the current chan + server
privmsg :: T.Text -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: T.Text -> T.Text -> Net ()
write s t = do
    let st = s ++ " " ++ t
    h <- asks $ view socket
    io $ F.hprint h "{}\r\n" $ F.Only st
    io $ F.print    "> {}\n" $ F.Only st

-- Convenience.
io :: IO a -> Net a
io = liftIO
