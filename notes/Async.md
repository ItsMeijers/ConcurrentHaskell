# Async without Exceptions

An example implementation of Async without exceptions:

```haskell
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    forkIO (do r <- action; putMVar var r)
    return (Async var)

-- readMvar allows multiple wait calls to be made for the same Async
wait :: Async a -> IO a
wait (Async var) = readMVar var 
```

Usage in downloading multiple websites with Async:

```haskell
twoLengthSite :: IO ()
twoLengthSite = do
    a1 <- async (getURL "http://wikipedia.com/wiki/Shovel")
    a2 <- async (getURL "http://wikipedia.com/wiki/Shovel")
    r1 <- wait a1
    r2 <- wait a2
    print (B.length r1, B.length r2)

sites = [ "http://www.google.com"
        , "http://www.wikipedia.com"
        , "http://www.bol.com"
        , "http://www.bing.com"
        , "http://www.yahoo.com"
        ]

timeDownload :: String -> IO ()
timeDownload url = do
    (page, time) <- timeit $ getURL url
    printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

downloadSites :: IO ()
downloadSites = do
    as <- mapM (async . timeDownload) sites
    mapM_ wait as
```

# Async with Exceptions

In Async, the natural behaivor would be for the error to be made available to the thread that calls
wait because that way the caller can find out whether the asynchronous computation return an error or
a result and act accordingly.

```haskell
-- | Make the possibility of an exception explicit in the type of MVar
data Async a = Async ThreadId (Mvar (Either SomeException a))

-- | try wraps it in the Either
async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    t   <- forkIO (do r try action; putMVar var r)
    return (Async t var)

-- | the Handler can handle the error immediately
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var

-- | Error is propegated to the caller of wait!
wait :: Async a -> IO a
wait a = do
    r <- waitCatch a
    case r of
        Left e  -> throwIO e
        Right a -> return a

-- | The TheadKilled exception is provided by the Control.Exception library and is
-- typically used in this way. 
cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled
```

_The only way to ignore an error is to ignore the result as well (Good library design?)_.

More functions on Async for merging results:

```haskell
waitEither :: Async a -> Async b -> Async (Either a b)
waitEither a b = do
    m <- newEmptyMVar
    forkIO $ do r <- try (fmap Left (wait a)); putMVar m r
    forkIO $ do r <- try (fmap Right (wait b)); putMVar m r
    wait (Async m)

waitAny :: [Async a] -> IO a
waitAny as = do
    m <- newEmptyMVar
    let forkwait a = forkIO $ do r <- try (wait a); putMVar m r
    mapM_ forkwait as
    wait (Async m)
```

A better version of donwload sites:

```haskell
main :: IO ()
main = do
    let
        download url = do
            r <- getURL url
            return (url, r)
    as <- mapM (async . download) sites
    (url, r) <- waitAny as
    printf "%s was first (%d bytes)\n" url (B.length r)
    mapM_ wait as
```

A version including cancellation:

```haskell
main :: IO ()
main = do
    as <- mapM (async . timeDownload) downloadSites
    forkIO $ do
        hSetBuffering stdin NoBuffering
        forever $ do
            c <- getChar
            when (c == 'q') $ mapM_ cancel as
    rs <- mapM waitCatch as
    printf "%d/%d succeeded\n" (length (right ts)) (length rs)
```

__Note__ that this works even though the program is sitting atop a large and complicated HTTP library
that provides no direct support for either cancellation or asynchronous I/O. Haskell's support for
cancellation is modular in this respect; most library code needs to do nothing to support it, although 
there are some simple and unintrusive rules that need to be followed when dealing with state (_see Cancellation and Timeouts_).