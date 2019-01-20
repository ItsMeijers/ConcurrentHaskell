{-# LANGUAGE TupleSections #-}

module ConnectionPool where
    
import Control.Concurrent
import System.Random
import Control.Monad
import Control.Concurrent.Chan

-- import Data.Text

data PoolSettings = 
    PoolSettings
        { connectionSettings :: !ConnectionSettings
        , poolSize           :: !Int
        } deriving (Show, Eq)

data ConnectionSettings = ConnectionSettings
        { host     :: !String -- TODO change in text
        , port     :: !Int
        , maxRetry :: !Int
        , jobSize  :: !Int
        } deriving (Show, Eq)

data ConnectionPool = ConnectionPool (Int, MVar Connection) (Chan (Job, MVar Int))

execute :: ConnectionPool -> Job -> IO Int
execute cp job = do
    _         <- putStrLn "Executing job"
    resultVar <- newEmptyMVar
    _         <- addJob cp (job, resultVar)
    takeMVar resultVar

emptyConnectionPool :: PoolSettings -> IO ConnectionPool
emptyConnectionPool poolSettings = do
    connections        <- emptyConnection (connectionSettings poolSettings)
    --traverse (\x -> fmap (x,) newEmptyMVar) [1..poolSize poolSettings]
    jobQueue           <- newChan
    let connectionPool = ConnectionPool (1, connections) jobQueue
    _ <- forkIO (forever $! scheduleJob connectionPool)
    return connectionPool

-- | Continue schedule new jobs when a Connection is Idle
scheduleJob :: ConnectionPool -> IO ()
scheduleJob cp@(ConnectionPool (n, mc) jobQueue) = do
    connection <- takeMVar mc
    case connection of
        Connection i Running jobs connectionSettings -> do
            putMVar mc (Connection i Running jobs connectionSettings)
            return ()
        Connection i Idle jobs connectionSettings -> do
            job <- readChan jobQueue
            writeChan jobs job
            if i + 1 == jobSize connectionSettings
                then putMVar mc (Connection (i + 1) Running jobs connectionSettings)
                else putMVar mc (Connection (i + 1) Idle jobs connectionSettings)
            return ()

addJob :: ConnectionPool -> (Job, MVar Int) -> IO ()
addJob (ConnectionPool _ jobQueue) = writeChan jobQueue

data Connection = Connection Int ConnectionState (Chan (Job, MVar Int)) ConnectionSettings
                deriving Eq

instance Show Connection where
    show (Connection _ state _ settings) = "Connection in state: " ++ show state

emptyConnection :: ConnectionSettings -> IO (MVar Connection)
emptyConnection cs =  do
    jobChannel <- newChan
    let connection = Connection 0 Idle jobChannel cs
    connectionV <- newEmptyMVar
    _ <- putMVar connectionV connection
    _ <- forkIO (forever $! runJobs connectionV)
    return connectionV

runJobs :: MVar Connection -> IO ()
runJobs connectionV = do
    connection <- takeMVar connectionV
    case connection of
        co@(Connection n Idle jobChannel cs) -> do
            putMVar connectionV connection
            return ()
        co@(Connection n Running jobChannel cs) -> do
            _        <- putStrLn ("Running " ++ show n ++ " jobs on channel 1")
            jobs     <- getChanContents jobChannel
            _ <- mapM_ (uncurry executeJob) jobs
            jobChannel <- newChan
            let connection = Connection 0 Idle jobChannel cs
            _ <- putMVar connectionV connection
            return ()

executeJob :: Job -> MVar Int -> IO ()
executeJob (n, job) resultVar = do
    result <- job
    _ <- putStrLn ("Just executed job: " ++ show n)
    putMVar resultVar result
    putStrLn "put in the mvar"

data ConnectionState = Idle
                     | Running
                     deriving (Show, Eq)

type Job = (Int, IO Int)

testJob :: Int -> Job
testJob i = (i, do
    t <- randomRIO (5000, 40000)
    _ <- threadDelay t
    putStrLn $ "Finished a random job in ms: " ++ show t
    return t)

mainTest :: IO ()
mainTest = do
    connectionPool <- emptyConnectionPool (PoolSettings (ConnectionSettings "" 3333 1 1) 1)
    mapM_ (fmap print . execute connectionPool . testJob) [1..200]
            
            