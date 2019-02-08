# Unbounded Channels based on MVar

(Also available in the `Control.Concurrent` module)

```haskell
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data MyChan a
    = MyChan (MVar (Stream a)) -- read pointer
           (MVar (Stream a))   -- write pointer
```

The end of the `Stream` is represented by an empty `MVar` called the _hole_, into which
the next value to be written to the channel will be p

```haskell
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
    Item val tail <- readMVar stream
    putMVar readVar tail
    return val

```

If multiple theads concurrently call `readChan`, the first one will successfully call `takeMVar`
on the read end, but the subsequent threads will al lblock at this point until the first thread
completes the operation and updates the read end. If multiple threads call `writeChan`, a similair thing happens: the write end of the `Chan` is the synchronization point, allowing only
one thread at a time to add an item to the channel. However, the read and write ends, being
seperate `MVars`, allow concurrent `readChan` and `writeChan` operation to proceed without
interference.

```haskell
dupChan :: MyChan a -> IO (MyChan a)
dupChan (MyChan _ writeVar) = do
    hole <- readMVar writeVar  -- Uses the same write pointer as the original channel
    newReadVar <- newMVar hole -- Creates a new read pointer
    return (MyChan newReadVar writeVar) -- combines the two and returns it
```

This creates a duplicate channel (multicast) with the following semantics:

* The new MyChan begins empty
* Subsequent writes to Either Chan are read from both; that is reading an item from one Chan
does not remove it from the other.

```haskell
unGetChan :: MyChan a -> a -> IO ()
unGetChan (MyChan readVar _) val = do
    newReadEnd <- newEmptyMVar
    readEnd    <- takeMVar readVar
    putMVar newReadEnd (Item val readEnd)
    putMVar readVar newReadEnd
```

What happens when the channel is empty, a `readChan` is already waiting in a blocked state, and
another thread call `unGetChan`. The desired semantics is that `unGetChan` succeeds,
and `readChan` should return with the new element. What actually happens in this case is
deadlock. The thread blocked in readChan will be holding the read end MVar, and so the ungetChan
will also block in takeMvar trying to take the read end. There is no known implementation of
`unGetChan` based on this representation of Chan that has the desired semantics.

Moral of the story: `MVar`s can be difficult to work with in larger constructions (see STM).
