{-# LANGUAGE RankNTypes #-}

module Core (
  readCoreCount,
  readStats
  ) where

import           Control.Monad         (liftM)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8L

{-
A cpu stat consists of the following data:
     user    nice   system  idle      iowait irq   softirq  steal  guest  guest_nice
cpu  74608   2520   24433   1117073   6176   4054  0        0      0      0
-}

data Stat = Stat {
  statUser      :: Integer,
  statNice      :: Integer,
  statSystem    :: Integer,
  statIdle      :: Integer,
  statIOWait    :: Integer,
  statIRQ       :: Integer,
  statSoftIRQ   :: Integer,
  statSteal     :: Integer,
  statGuest     :: Integer,
  statGuestNice :: Integer
  } deriving (Show)

statFile :: String
statFile = "/proc/stat"

loadProcStat :: IO BS.ByteString
loadProcStat = BS.readFile statFile

readStats :: BS.ByteString -> [Stat]
readStats bs =
  map makeStat $ takeWhile (C8L.isPrefixOf (C8L.pack "cpu") . head . C8L.words) $ tail $ C8L.split '\n' bs
  where
    makeStat :: BS.ByteString -> Stat
    makeStat s = let
      ws = C8L.words s
      args = map (read . C8L.unpack) $ (take 10 . tail) ws :: [Integer]
      suser:snice:ssystem:sidle:siowait:sirq:ssirq:ssteal:sguest:sgnice:_ = args
      in
        Stat suser snice ssystem sidle siowait sirq ssirq ssteal sguest sgnice

readCurrentStats :: IO [Stat]
readCurrentStats = readStats <$> loadProcStat

readCoreCount :: BS.ByteString -> Int
readCoreCount = length . readStats

calcCPUUsageBetweenStats :: (Fractional a) => Stat -> Stat -> a
calcCPUUsageBetweenStats prev curr = let
  (prevBusy, prevIdle) = getBusyIdle prev
  (currBusy, currIdle) = getBusyIdle curr
  busyd = currBusy - prevBusy
  idled = currIdle - prevIdle
  in
    fromIntegral busyd / fromIntegral (busyd + idled)
  where
    getBusyIdle stat = (
      statUser stat + statNice stat + statSystem stat + statIRQ stat + statSoftIRQ stat + statSteal stat,
      statIdle stat + statIOWait stat
                       )
