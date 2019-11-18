import Data.Time.Clock
import Data.Time.Clock.POSIX

import Control.Concurrent(threadDelay)
import Control.Monad.State(runState)
import System.Random(randomRIO)
import MersenneTwister(mtInt, MTState, seedMt)

low = 40
high = 1000

randomNumberOfSeconds :: IO Int
randomNumberOfSeconds = randomRIO (low, high)

waitNSeconds :: Int -> IO ()
waitNSeconds n = threadDelay $ n * 10^6

secondsSinceEpoch :: UTCTime -> Int
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getSecondsSinceEpoch :: IO Int
getSecondsSinceEpoch = do
    now <- getCurrentTime
    return $ secondsSinceEpoch now
    
getTimeDependentRandomNumber :: IO (Int, MTState)
getTimeDependentRandomNumber = do
    nSeconds <- randomNumberOfSeconds
    waitNSeconds nSeconds
    seconds <- getSecondsSinceEpoch
    let seed = seedMt seconds
        (n, state) = runState mtInt seed
    nSeconds' <- randomNumberOfSeconds
    waitNSeconds nSeconds'
    
    return (n, state)

crackMTSeedHelper :: Int -> Int -> Int -> Maybe MTState
crackMTSeedHelper start last target
  | start == last = Nothing
  | guess == target = Just thisMtState
  | otherwise = crackMTSeedHelper (start+1) last target
    where
        (guess, thisMtState) = runState mtInt $ seedMt start
    

crackMTSeed :: Int -> IO (Maybe MTState)
crackMTSeed target = do
    now <- getSecondsSinceEpoch
    return $ crackMTSeedHelper (now - (3*high)) now target

main :: IO ()
main = do
    print $ "getting random number..."
    (n, actual) <- getTimeDependentRandomNumber
    startTime <- getCurrentTime
    print $ "starting to crack: " ++ show startTime
    crack <- crackMTSeed n
    endTime <- getCurrentTime
    print $ "finished crack: " ++ show endTime
    print $ fmap (==actual) crack
