# Threads

Forking a new thread of control:

```haskell
forkIO :: IO () -> IO ThreadId
```

If the thread has effects, those effects will be interleaved in an indeterminate fashion with the effects from other threads.

Deloying a thread which takes an argument representing a number of microseconds an waits for that amount of time before returning.

```haskell
threadDelay  :: Int -> IO ()
```

Reminder example:

```haskell
import Control.Concurrent
import Text.Printf
import Control.Monad

main =
    forever $ do
        s <- getLine
        forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
    let t = read s :: Int
    printf "Ok, I';; remind yo in %d seocnds\n" t
    threadDelay (10^6 * t)
    printf "%d seconds is is up! BING!"
```

Haskell programs terminate when main returns, even if there are other threads still running. The other threads simply stop running and cease to exist after main returns (See MVars logging example how to wait).