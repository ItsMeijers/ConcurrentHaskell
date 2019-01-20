module Channel where

import Control.Concurrent

-- TODO COMMENT!
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

-- TODO COMMENT!
data MyChan a
    = MyChan (MVar (Stream a)) -- read pointer
           (MVar (Stream a)) -- write pointer

-- TODO COMMENT!
newChan :: IO (MyChan a)
newChan = do
    hole     <- newEmptyMVar
    readVar  <- newMVar hole
    writeVar <- newMVar hole
    return (MyChan readVar writeVar)

-- TODO COMMENT!
writeChan :: MyChan a -> a -> IO ()
writeChan (MyChan _ writeChan) val = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeChan
    putMVar oldHole (Item val newHole)

-- TODO COMMENT!
readChan :: MyChan a -> IO a
readChan (MyChan readVar _) = do
    stream        <- takeMVar readVar
    Item val tail <- readMVar stream -- todo readMVar comment instead of takeMVar
    putMVar readVar tail
    return val

-- | This creates a duplicate channel with the following semantics:
-- * The new MyChan begins empty
-- * Subsequent writes to Either Chan are read from both; that is
--   reading an item from one CHan does not remove it from the other.
dupChan :: MyChan a -> IO (MyChan a)
dupChan (MyChan _ writeVar) = do
    hole <- readMVar writeVar  -- Uses the same write pointer as the original channel
    newReadVar <- newMVar hole -- Creates a new read pointer
    return (MyChan newReadVar writeVar) -- combines the two and returns it


-- TODO COMMENT!
unGetChan :: MyChan a -> a -> IO ()
unGetChan (MyChan readVar _) val = do
    newReadEnd <- newEmptyMVar
    readEnd    <- takeMVar readVar
    putMVar newReadEnd (Item val readEnd)
    putMVar readVar newReadEnd