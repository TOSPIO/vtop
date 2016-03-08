{-# LANGUAGE RankNTypes #-}

module Core (
  readCoreCount,
  readStats
  ) where

import qualified Control.Concurrent.Timer as Timer
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8L
import           Data.Default
import           Data.IORef
import           System.IO.Unsafe
import           Data.Int

{-
A cpu stat consists of the following data:
     user    nice   system  idle      iowait irq   softirq  steal  guest  guest_nice
cpu  74608   2520   24433   1117073   6176   4054  0        0      0      0
-}

data Stat = Stat {
  statUser      :: Int64,
  statNice      :: Int64,
  statSystem    :: Int64,
  statIdle      :: Int64,
  statIOWait    :: Int64,
  statIRQ       :: Int64,
  statSoftIRQ   :: Int64,
  statSteal     :: Int64,
  statGuest     :: Int64,
  statGuestNice :: Int64
  } deriving (Show)

defaultStat :: Stat
defaultStat = Stat 0 0 0 0 0 0 0 0 0 0

defaultDelay :: Int64
defaultDelay = 1000

type StatPair = (Stat, Stat)

push :: StatPair Stat
push (l, r) n = (r, n)

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
      args = map (read . C8L.unpack) $ (take 10 . tail) ws :: [Int64]
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


{-# NOINLINE timeSlice #-}
timeSlice :: IORef Int64
timeSlice = unsafePerformIO $ newIORef def

incubateStats :: IO [[Stat]]
incubateStats = do
  repeatedTimer (msDelay defaultDelay)
