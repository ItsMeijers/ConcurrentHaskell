# Unbounded Channels based on MVar

(Also available in the `Control.Concurrent` module)

```haskell
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data MyChan a
    = MyChan (MVar (Stream a)) -- read pointer
           (MVar (Stream a))   -- write pointer

newChan :: IO (MyChan a)
newChan = do
    hole     <- newEmptyMVar
    readVar  <- newMVar hole
    writeVar <- newMVar hole
    return (MyChan readVar writeVar)

writeChan :: MyChan a -> a -> IO ()
writeChan (MyChan _ writeChan) val = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeChan
    putMVar oldHole (Item val newHole)

readChan :: MyChan a -> IO a
readChan (MyChan readVar _) = do
    stream        <- takeMVar readVar
    Item val tail <- takeMVar stream
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
```