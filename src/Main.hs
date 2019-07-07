{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when, unless)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS hiding (putStrLn)
import qualified Data.ByteString.Char8 as BSC
import Network.Socket hiding (recv)
import Network.Socket.ByteString

import Text.Parsec hiding (space)
import Text.Parsec.ByteString (Parser)

import Control.Exception (catch)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Directory (removeFile)
import System.Random (randomRIO)

botName :: String
botName = "jsynacek-lambda-bot"

pass, nick, user :: ByteString
pass = msg "PASS totallysecret"
nick = msg $ "NICK " <> BSC.pack botName
user = msg $ "USER " <> BSC.pack botName <> " jsynacek-ntb jsynacek-ntb :Jan Synacek's Friendly Bot"

msg, pong, join, part, mesg, quit :: ByteString -> ByteString
msg = (<> "\r\n")
pong = msg . ("PONG " <>)
join = msg . ("JOIN " <>)
part = msg . ("PART " <>)
mesg = msg . ("PRIVMSG " <>)
quit = msg . ("QUIT " <>)

mesgTo :: ByteString -> ByteString -> ByteString
mesgTo to = mesg . (to <>) . (" :" <>)

main :: IO ()
main = do
  addr:_ <- getAddrInfo Nothing (Just "irc.freenode.net") (Just "6667")
  sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  sendAll sock pass
  sendAll sock nick
  sendAll sock user
  fortunes <- loadFortunes
  _ <- forkIO $ commander sock
  loop sock "" fortunes
  where
    loop :: Socket -> ByteString -> [String] -> IO ()
    loop sock part fortunes = do
      recvd <- recv sock 1024
      unless (BS.null recvd) $ do
        rest <- process (part <> recvd) $ \m -> do
          case parse message "(stream)" m of
            Left  _  -> BSC.putStr $ "!! error: malformed msg: " <> m
            Right m' -> do
              putStrLn $ show m'
              respond sock m' fortunes
        loop sock rest fortunes
    -- Break up the receive buffer to potential messages.
    -- Return the beginning of the next message if any.
    process :: ByteString -> (ByteString -> IO ()) -> IO ByteString
    process buf act = do
      let (part, rest) = BS.breakSubstring "\r\n" buf
      if BSC.null rest
        then pure part
        else act (part <> "\r\n") >> process (BS.drop 2 rest) act

respond :: Socket -> IRCMessage -> [String] -> IO ()
respond sock (IRCMessage (Just pref) "PRIVMSG" (to:msg:_)) fortunes = do
  when (to == botName) $ do
    let from = takeWhile (/= '!') pref
        cmd  = takeWhile (/= ' ') $ drop 1 msg
    f <- giveFortune fortunes
    sendAll sock $ mesgTo (BSC.pack from) $
      if | cmd == "!fortune" -> BSC.pack f
         | cmd == "!dict" -> "TBI Real Time Soon!"
         | otherwise -> "Hi, I'm a friendly IRC bot. Type !fortune to get a fortune cookie."
respond sock (IRCMessage _ "PING" (s:_)) _ = sendAll sock $ pong $ BSC.pack s
respond _ _ _ = pure ()

---------------------------------------------------------------------------------------------------
-- Parser
-- https://tools.ietf.org/html/rfc1459#section-2.3.1
----------------------------------------------------
data IRCMessage =
  IRCMessage { ircMsgPrefix  :: Maybe String
             , ircMsgCommand :: String
             , ircMsgParams  :: [String]
             } deriving Show

message :: Parser IRCMessage
message = IRCMessage <$> option Nothing (Just <$> prefix) <*> command <*> params <* crlf

prefix :: Parser String
prefix = char ':' *> many (noneOf " ") <* space

-- nickp :: Parser String
-- nickp = (:) <$> letter <*> many1 (try letter <|> try digit <|> special)

-- userp :: Parser String
-- userp = many1 $ noneOf " \x00\r\n"

-- special :: Parser Char
-- special = oneOf "-[]\\`^{}"

space :: Parser ()
space = skipMany1 $ char ' '

command :: Parser String
command = try (many1 letter) <|> count 3 digit

params :: Parser [String]
params = many (space *> (try ((:) <$> char ':' <*> trailing) <|> middle))
  where
    trailing = many $ noneOf "\x00\r\n"
    middle   = (:) <$> noneOf ":" <*> many (noneOf " \x00\r\n")

---------------------------------------------------------------------------------------------------
-- Command dispatch
-------------------
commander :: Socket -> IO ()
commander serverSock = prepareSocket >>= loop
  where
    loop sock = do
      (conn, _) <- accept sock
      loop' conn
      loop sock
      where
        loop' conn = do
          recvd <- recv conn 1024
          unless (BS.null recvd) $ do
            maybe (sendAll conn $ "unknown command: " <> recvd)
                  (\act -> act serverSock $ BS.drop 6 recvd)
                  (Map.lookup (BSC.takeWhile (/= ' ') $ BSC.dropWhile (== ' ') recvd) dispatch)
            loop' conn
    prepareSocket = do
      s <- socket AF_UNIX Stream 0
      catch (removeFile socketPath) handle
      bind s $ SockAddrUnix socketPath
      listen s 1
      pure s
    handle :: IOError -> IO ()
    handle _ = pure ()
    -- Dedicated to all the security folks out there.
    socketPath = "/tmp/lambda-irc.sock"

dispatch :: Map ByteString (Socket -> ByteString -> IO ())
dispatch = Map.fromList [ ("/mesg", \s -> sendAll s . mesg)
                        , ("/join", \s -> sendAll s . join)
                        , ("/part", \s -> sendAll s . part)
                        , ("/quit", \s -> sendAll s . quit)
                        ]

---------------------------------------------------------------------------------------------------
-- Fortunes
-----------
-- Load fortunes from '$HOME/.fortunes'. If it can't be read for some reason,
-- a few hard-coded fortune lines are provided.
loadFortunes :: IO [String]
loadFortunes = catch (getEnv "HOME" >>= pure . (</> ".fortunes") >>= readFile >>= pure . lines)
                     handle
  where handle :: IOError -> IO [String]
        handle _ = pure someFortunes

giveFortune :: [String] -> IO String
giveFortune fs = do
  -- Not very efficient, but good enough.
  i <- randomRIO (0, length fs - 1)
  pure (fs !! i)

someFortunes :: [String]
someFortunes =
  [ "The fortune you seek is in another cookie."
  , "A conclusion is simply the place where you got tired of thinking."
  , "A foolish man listens to his heart. A wise man listens to cookies."
  , "Never forget a friend. Especially if he owes you."
  , "I am worth a fortune."
  ]