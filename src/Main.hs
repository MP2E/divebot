{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
import Data.List
import Network
import System.IO
import System.Exit
import GHC.Generics
import Control.Lens
import System.Random
import Data.Time.Clock
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Map.Strict ((!))
import Control.Arrow (first)
import Control.Monad (unless, when)
import Text.Printf (hPrintf,printf)
import Data.Serialize (encode, decode)

import qualified Data.Set        as S
import qualified Data.Serialize  as C
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

server = "irc.oftc.net"
port   = 6667
chan   = "#noteternityenginerelated"
nick   = "divebot"

data Bot = Bot { _socket :: Handle, _starttime :: UTCTime }
makeLenses ''Bot

data ChatMap = ChatMap { _markov :: Map.Map [String] (S.Set String), _entryDb :: S.Set String } deriving Generic
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
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetBuffering stdout LineBuffering -- Needs to be explicit under Windows!
    hSetEncoding stdout utf8
    return (Bot h t)
  where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

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
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    ping x        = "PING :" `isPrefixOf` x
    pong x        = write "PONG" (':' : drop 6 x)
    clean         = drop 1 . unwords . drop 3 . words . cleanStatus
    cleanStatus x = if cleanPred $ x then [] else x -- remove anything that satisfies cleanPred
    cleanPred x   = (drop 3 server `isInfixOf` x) ||
                    ((nick ++ "!~" ++ nick) `isInfixOf` x) ||
                    ("JOIN :" `isInfixOf` x)  || ("PART :" `isInfixOf` x) ||
                    ("QUIT :" `isInfixOf` x)  || ("MODE" `isInfixOf` x)

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"                      = write "QUIT" ":Exiting" >> io exitSuccess
eval     "!uptime"                    = uptime >>= privmsg
eval     "!loadstate"                 = readBrain            -- read from file and deserialize markov chain
eval     "!savestate"                 = writeBrain           -- serialize markov chain and write to file
eval     "!parsefile"                 = privmsg "error: enter servername/#channel.log to parse"
eval []                               = return ()            -- ignore, empty list indicates a status line
eval x | "!parsefile " `isPrefixOf` x = parseChatLog x       -- parse an irssi chatlog to create an initial markov state
eval x | "!id " `isPrefixOf` x        = privmsg (drop 4 x)
eval x                                = do
    let xs = (removeLinks . words) x
    i <- io $ getStdRandom $ randomR (0,99) :: Net Int
    when ((i<4) || (nick `isInfixOf` x)) markovSpeak
    updateEntryDb xs
    createMarkov xs

removeLinks :: [String] -> [String]
removeLinks = filter $ \x -> not $ ("http://" `isPrefixOf` x) || ("https://" `isPrefixOf` x)

updateEntryDb :: [String] -> Net ()
updateEntryDb [] = return () -- parseChatLog passes [] if it parses a status line
updateEntryDb xs = modify $ over entryDb $ S.insert (head xs)

parseChatLog :: String -> Net ()
parseChatLog x  = do
    let statusPred x = ("---" `isPrefixOf` x)    || ("-!-" `isInfixOf` x) -- remove lines matching these predicates
        clean x = if statusPred $ x then [] else drop 3 $ words x
        f = drop 11 x
    rawlog <- io $ catch (readLines ("/home/cray/irclogs/" ++ f)) (\e -> do let err = show (e :: IOException)
                                                                            hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                                                                            return [])
    if null rawlog
    then
        privmsg "parse error, chatlog not found or inaccessible"
    else do
        sequence_ $ fmap (\y -> let ys = (removeLinks . clean) y in updateEntryDb ys >> createMarkov ys) rawlog
        privmsg "successfully parsed!"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

writeBrain :: Net ()
writeBrain = do
    c          <- get
    let contents = encode c
    fileHandle <- io $ openBinaryFile "markov_brain.brn" WriteMode
    io $ hSetBuffering fileHandle NoBuffering
    io $ BS.hPut fileHandle contents

readBrain :: Net ()
readBrain = do
    fileHandle <- io $ openBinaryFile "markov_brain.brn" ReadMode
    io $ hSetBuffering fileHandle NoBuffering
    contents   <- io $ BS.hGetContents fileHandle
    either (io . putStrLn) put $ decode contents

-- wrapper around markov sentence generation
markovSpeak :: Net ()
markovSpeak = do
    c        <- get
    start    <- io . searchMap (view entryDb c) $ Map.keys (view markov c)
    sentence <- io $ assembleSentence c start
    unless (null sentence) $ privmsg sentence

-- grabs random entry-point from entryDb and returns all possible keys
searchMap :: S.Set String -> [[String]] -> IO [[String]]
searchMap xs k | S.null xs = return []
searchMap xs k             = do
    let m = length (S.toList xs) - 1
    startPoint <- getStdRandom $ randomR (0,m)
    return $ filter (((startPoint `S.elemAt` xs) ==) . head) k

-- assembles the sentence, taking random paths if the sentence branches
assembleSentence :: ChatMap -> [[String]] -> IO String
assembleSentence c [] = return []
assembleSentence c xs = do
    let m = length xs - 1
    startPoint <- getStdRandom $ randomR (0,m)
    fmap unwords $ subAssembler $ xs !! startPoint
  where
    subAssembler :: [String] -> IO [String]
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
createMarkov :: [String] -> Net ()
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

uptime :: Net String
uptime = do
    now  <- io getCurrentTime
    zero <- asks $ view starttime
    return . pretty $ diffUTCTime now zero

-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: NominalDiffTime -> String
pretty td =
  unwords $ fmap (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (floor td,[]) metrics

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks $ view socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO
