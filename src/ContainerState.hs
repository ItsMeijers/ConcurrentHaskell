module ContainerState where

import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as Map

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
    let book' = Map.insert name number book
    putMVar m book'
    seq book' (return ()) -- Otherwise the mvar becomes a big Thunk

lookUp :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookUp (PhoneBookState m) name = do
    book <- takeMVar m
    putMVar m book
    return (Map.lookup name book)

phoneBookExample :: IO ()
phoneBookExample = do
    s <- new
    sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
    lookUp s "name999" >>= print
    lookUp s "unkown" >>= print