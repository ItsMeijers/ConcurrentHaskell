# MVars

`MVar` is the basic _communication_ mechanism provided by Concurrent Haskell.

An Mvar can be thought of as a box that is either empty or full. The `newEmptyMVar` operation creates a new empty box, and `newMVar` creates a new full box containing the value passed as its argument. The `takeMVar` operation removes the value from a full `MVar` and returns it, but blocks if the `MVar` is currently empty. The `putMVar` operation puts a value into the `MVar` but blocks if the `MVar` is already full.

The API for Mvar:

```haskell
data MVar a

newEmptyMVar :: IO (MVar a)
newMVar :: a -> IO (MVar a)
takeMVar :: MVar a -> IO a
putMVar :: Mvar a -> a -> IO ()
```

The runtime system detects when operations are blocked forever and throws a special exception called `BlockIndefinitelyOnMVar`.

Example use cases for `MVar`:

* An `MVar` is a _one-place channel_, which means that it can be used for passing messages between threads, but it can hold at most one message at a time.
* An `MVar` is a _container for mutable state_.
* An `Mvar` is a _building block_ for constructing larger concurrent datastructures.

## Example: Mvar as a Simple Channel: A logging service

```haskell
-- | Having a Logger ba value that we pass around rather than a globally known top-level value is good practice. For example this allows multiple loggers.
data Logger = Logger (MVar LogCommand)

-- | The MVar in the stop message allows the caller of logStop to wait until the logging thread indacites it has finished (solution to not having the main thread kill still running threads when it returns).
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let l = Logger m
    forkIO (logger l) -- Fork the thread to perform the service
    return l

logger :: Logger -> IO ()
logger (Logger m) = loop
    where
        loop = do
            cmd <- takeMVar m
            case cmd of
                Message msg -> do
                    putStrLn msg
                    loop
                Stop s -> do
                    putStrLn "logger: stop"
                    putMVar s () -- reply to logStop

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s

logExample :: IO ()
logExample = do
    l <- initLogger
    logMessage l "hello"
    logMessage l "bye"
    logStop l
```

The `MVar` is only a one place channel, so therefore if multiple threads where writing to this `Logger` it would probably not able to process everything quickly enough and get block in `logMessage`.

## MVar as a Container for Shared State

`MVar` provides the combination fo a lock and a mutable variable in Haskell. To acquire the lock, we take the `MVar`, whereas, to update the variable and release the lock, we put the `MVar`.

```haskell
type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

-- Good practice to wrap the MVar in a new/data type since the clients that use this interface do not depend or see the underlying implementation.
newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
    m <- newMVar Map.empty
    return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
    book <- takeMVar m
    putMVar m (Map.insert name number book)

lookUp :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookUp (PhoneBookState m) name = do
    book <- takeMVar m
    let book' = Map.insert name number book
    putMVar m book'
    seq book' (return ()) -- Otherwise the mvar becomes a big Thunk

phoneBookExample :: IO ()
phoneBookExample = do
    s <- new
    sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
    lookUp s "name999" >>= print
    lookUp s "unkown" >>= print
```

With this sequence, we are storing an unevaluated expression in the `MVar`, but it is evaluated 
immediately after the putMVar. The lock is held only briefly, but not the thunk is also
evaluated so we avoid building a long chain of thunks.

## Fairness

No thead can be blocked _indefinitly_ on an `MVar` unless another thread holds that MVar
_indefinitly_. Fairness allows the winning thread its operatino and calls putMVar, the scheduler
wakes up the blocked thead and completes its blocked takeMVar, so the original winnig thread will
immediately block when it tries to reacquire the handle. Hence this leads to perfect alternation
the two threads. The only way that the alternation pattern can be broken if one thread is
descheduled while it is not holding the `MVar`.

A consequence of the fairness implemenation is that, when multiple threads are blocked in
`takeMVar` and another thread does a `putMVar`, only one of the blocked threads becomes unblocked.
This "single wakeup" property is a particularly important performance charasteristics when a
large number of threads are contending for a single `MVar` The fairness guarantee together with
the single wakeup property that keeps `MVar`s from being completely subsumed by software
transaction memory.