{-# LANGUAGE BangPatterns #-}
import Data.List
import Network
import System.IO
import System.Exit
import System.Time
import System.Random
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Data.Map.Strict ((!))
import Control.Arrow (first)
import Control.Monad (unless)
import Control.Applicative ((<$>))
import Text.Printf (hPrintf,printf)
import Data.Serialize (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Control.Exception (bracket,bracket_)
import Data.Maybe (maybeToList, isNothing, fromJust)

server = "irc.oftc.net"
port   = 6667
chan   = "#mp2e-testing"
nick   = "divebot"

-- The 'Net' type, a wrapper over Reader, State, and IO.
type Net = ReaderT Bot (StateT ChatMap IO)
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
type ChatMap = Map.Map [String] [String]

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop r     = evalStateT (runReaderT run r) Map.empty

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetBuffering stdout LineBuffering -- Needs to be explicit under Windows!
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
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    ping x        = "PING :" `isPrefixOf` x
    pong x        = write "PONG" (':' : drop 6 x)
    clean         = drop 1 . dropWhile (/= ':') . cleanStatus . drop 1
    cleanStatus x = if cleanPred x then [] else x -- remove joins, mode changes, and server notifications
    cleanPred x   = ( drop 3 server `isInfixOf` x ) || ( (nick ++ "!~" ++ nick) `isInfixOf` x )
                    || ( "JOIN :" `isInfixOf` x ) -- "dropWhile (/= ':')" in clean removes PARTs

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"                      = write "QUIT" ":Exiting" >> io exitSuccess
eval     "!uptime"                    = uptime >>= privmsg
eval     "!loadstate"                 = readBrain            -- read from file and deserialize markov chain
eval     "!savestate"                 = writeBrain           -- serialize markov chain and write to file
eval     "!getstate"                  = get >>= (io . print) -- debug function, print markov chain to stdout
eval []                               = return ()            -- ignore, empty list indicates a status line
eval x | "!parsefile " `isPrefixOf` x = parseChatLog x        -- parse an irssi chatlog to create an initial markov state
eval x | "!id " `isPrefixOf` x        = privmsg (drop 4 x)
eval x                                = (markovSpeak . words) x >> (createMarkov  . words) x

parseChatLog :: String -> Net ()
parseChatLog [] = privmsg "error: enter servername/#channel.log to parse"
parseChatLog x  = do
    let statusPred x = ("---" `isPrefixOf` x) || ("-!-" `isInfixOf` x) -- remove lines matching these predicates entirely
        clean x = if statusPred x then [] else (drop 3 $ words x)
        f = drop 11 x
    rawlog <- io $ catch (readLines ("/home/cray/irclogs/" ++ f)) (\e -> do let err = show (e :: IOException)
                                                                            hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                                                                            return [])
    if null rawlog
    then
        privmsg "parse error, chatlog not found or inaccessible"
    else do
        sequence_ $ fmap (createMarkov . clean) rawlog
        privmsg "successfully parsed!"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

writeBrain :: Net ()
writeBrain = do
    c          <- get
    let !contents = encode c
    fileHandle <- io $ openBinaryFile "markov_brain.txt" WriteMode
    io $ hSetBuffering fileHandle NoBuffering
    io $ BS.hPut fileHandle contents

readBrain :: Net ()
readBrain = do
    fileHandle <- io $ openBinaryFile "markov_brain.txt" ReadMode
    io $ hSetBuffering fileHandle NoBuffering
    contents   <- io $ BS.hGetContents fileHandle
    either (io . putStrLn) put $ decode contents

-- wrapper around markov sentence generation
markovSpeak :: [String] -> Net ()
markovSpeak s = do
--  io $ getStdRandom $ randomR (0,99) :: Net Int
--  unless (i>19) $ do
    c <- get
--  io . print $ searchMap s $ Map.keys c
    sentence <- io . assembleSentence c $ searchMap s $ Map.keys c
    unless (null sentence) $ privmsg sentence

-- assembles the sentence, taking random paths if the sentence branches
assembleSentence :: ChatMap -> [[String]] -> IO String
assembleSentence c [] = return []
assembleSentence c xs = do
    let m = length xs - 1
    !startPoint <- getStdRandom $ randomR (0,m)
    fmap unwords $ subAssembler $ xs !! startPoint
  where
    subAssembler :: [String] -> IO [String]
    subAssembler []     = return []
    subAssembler [y]    = return [y]
    subAssembler (y:ys) = fmap (y:) $ do
        let !y2     = head ys
            !values = Map.lookup [y, y2] c
            !m2     = case values of
                          Nothing -> -1
                          Just z  -> length z - 1
        if m2 < 0
        then return [y2]
        else do
            !point    <- getStdRandom $ randomR (0,m2)
            subAssembler [y2, fromJust values !! point]

-- Looks through the input in order, searching the markov map for a corresponding beginning point
searchMap :: [String] -> [[String]] -> [[String]]
searchMap []     k = []
searchMap (x:xs) k = if null results then searchMap xs k else results
  where
    results = filter ((x==) . head) k

-- create the markov chain and store it in our ChatMap
createMarkov :: [String] -> Net ()
createMarkov [x]    = modify $ Map.insert [x] []
createMarkov (x:xs) = do
    modify $ Map.insertWithKey mergeValues key value
    createMarkov xs
  where key               = [x, head xs]
        value             = xs `chatIndex` 1
        chatIndex ys i    = maybeToList $ safeIndex ys i
        -- ignore the key passed from Map.insertWithKey
        mergeValues _ x y = if or ((==) <$> x <*> y) then y else y ++ x

-- safe version of (!!)
safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _         = Nothing
safeIndex _      n | n < 0 = Nothing
safeIndex (x:_)  0         = Just x
safeIndex (_:xs) n         = safeIndex xs (n-1)

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: TimeDiff -> String
pretty td =
  unwords $ fmap (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td,[]) metrics

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO
