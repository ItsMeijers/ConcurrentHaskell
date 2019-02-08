# Cancellation and Timeouts

The crucial design decision in supporting cancellation is whether the intended victim should have to
poll for the cancellation condition or whether the thread is immediately cancelled in some way.
This is a trade off:

* If the thread has to poll, then there is a danger that the programmer may forget to poll regularly
enough and the thread will become unresponsive, perhaps permanently so. (Lead to hangs and deadlocks)
* If cancellation happens asynchronously, critical sections that modify state need to be protected
cancellation. Cancellation may occur mid update, leaving some data in an inconsistent state.

The choice is between doing 1 or doing both 1 and 2. Purely functional code cannot poll for the
cancellation conditions, so it must be cancelllable by default. Fully asynchronous cancellation is the
only sensible default in Haskell, and the design problem reduces to deciding how cancellation is
handled by code in the IO monad.

## Asynchrounous Exceptions

There is a fundamental difference between the kind of exception thrown by `openFile` when the file
does not exist, for example, and an exception that may arise at any time because the user pressed the
"stop" button. We call the latter kind an _asynchronous exception_ because it is asynchronous from the
"victim", they didn't ask for it. Conversely, exceptions thrown using the normal thwo and throwIO are
called _synchronous exceptions_.

To iniate an _asynchronous exception_ (see async implementation):

```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()
```

## Masking Asynchronous

We need a way to control the delivery of asynchronous exceptions during critical sections.

```haskell
mask :: ((IO a -> IO a) -> IO b) -> IO b
```

The `mask` operation defers the delivery of asynchronous exceptions for the duration of its argument.

```haskell
problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
    a <- takeMVar m
    r <- restore (f a) `catch` e -> do putMVar m a; throw e
    putMVar m r
```

THe restore function can be used to restore the delivery of asynchronous exceptions to its present
state during execution of the argument to mask. Now an exception can be raised only while (f a) is
working and we have an exception handler to catch any exceptions in that case. A small number of
operations, including takeMVar, are designed as _interruptible_. Interruptible operations may
receive asynchronous exceptions even inside mask. __With mask, asynchronous exceptions become synchronous__.