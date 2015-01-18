{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Network
import System.IO
import System.Exit
import System.Time
import Control.Arrow (first)
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (bracket,bracket_)
import Text.Printf (hPrintf,printf)

server = "irc.oftc.net"
port   = 6667
chan   = "#mp2e-testing"
nick   = "divebot"

-- The 'Net' monad, a wrapper over Reader, State, and IO.
type Net = ReaderT Bot (StateT ChatLog IO)
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
type ChatLog = String

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop r     = evalStateT (runReaderT run r) []

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
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
                    || ( "JOIN :" `isInfixOf` x ) -- "PART" is not in the predicates because clean already removes them

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> io exitSuccess
eval     "!uptime"             = uptime >>= privmsg
eval     "!getstate"           = get >>= (io . putStr)   -- debug function, print chatlog to stdout
eval []                        = return ()               -- ignore, empty list indicates a status line
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x                         = modify (++ (x ++ "\n")) -- log chat input, line by line

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
