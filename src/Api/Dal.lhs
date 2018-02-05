Data Access Layer
=================

<!--
This puts the code in an HTML comment in the Pandoc output of this file.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
\end{code}
-->

We can use typeclasses in Haskell to specify interfaces for multiple types. We can use this to specify a set of actions for our DAL.

The typeclass is UserDal, and we have two implementations for it: a mock DAL and a SQL DAL using Sqlite.

\begin{code}
module Api.Dal
  ( UserDal(..)
  , MockUserDal
  , SqlUserDal
  , DalProvider
  , withProvider
  , initMockUserDal
  , initSqlUserDal
  ) where
\end{code}

<!--
\begin{code}
import Control.Concurrent.MVar
import Database.SQLite.Simple

import Api.Data
\end{code}
-->

Our interface is very simple. Any type implementing the `UserDal` interface provides these three function, which take a value of type `a` (a database resource), and performs an IO action with the result.

\begin{code}
class UserDal a where
  getUser :: UserId -> a -> IO (Maybe User)
  getAllUsers :: a -> IO [User]
  addUser :: NewUser -> a -> IO UserId
\end{code}

Rather than exporting relatively low-level constructors for each UserDal instance, we can manage database resources with another type, the `DalProvider`. This gives us flexibility in implementation and allows us to control how resources are used (for example, to prevent connections from being left open).

\begin{code}
data DalProvider d where
  MockUserDalProvider :: MockUserDal -> DalProvider MockUserDal
  SqlUserDalProvider :: DalProvider SqlUserDal
\end{code}

`withProvider` is the only way to use a database resource at the top level.

\begin{code}
withProvider :: DalProvider d -> (d -> IO a) -> IO a
withProvider (MockUserDalProvider dal) f = f dal
withProvider SqlUserDalProvider f = withConnection "file::memory:?cache=shared" (f . SqlUserDal)
\end{code}

We'll start with the mock DAL. It is simply a (thread safe) mutable IO variable with a list of users and a counter for the next user id.

\begin{code}
newtype MockUserDal = MockUserDal (MVar (Int, [User]))

initMockUserDal :: Int -> [User] -> IO (DalProvider MockUserDal)
initMockUserDal i us = do
  initialState <- MockUserDal <$> newMVar (i, us)
  return $ MockUserDalProvider initialState
\end{code}

The implementation.

\begin{code}
instance UserDal MockUserDal where
  getUser i (MockUserDal r) = safeHead . filter (\u -> userId u == i) . snd <$> readMVar r

  getAllUsers (MockUserDal r) = snd <$> readMVar r

  addUser (NewUser fn ln) (MockUserDal r) = do
    (nextId, users) <- takeMVar r
    let uid = UserId nextId
    putMVar r (nextId + 1, User uid fn ln : users)
    return uid
\end{code}

`safeHead` is a total version of the `head` function.

\begin{code}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
\end{code}

An SQLite implementation.

\begin{code}
newtype SqlUserDal = SqlUserDal Connection

initSqlUserDal :: IO (DalProvider SqlUserDal)
initSqlUserDal = do
  conn <- open "file::memory:?cache=shared"
  execute_ conn "CREATE TABLE user(user_id INTEGER PRIMARY KEY, first_name TEXT NOT NULL, last_name TEXT NOT NULL)"
  return SqlUserDalProvider

instance UserDal SqlUserDal where
  getUser i (SqlUserDal c) = safeHead <$> query c "SELECT user_id, first_name, last_name FROM user WHERE user_id = ?" (Only i)

  getAllUsers (SqlUserDal c) = query_ c "SELECT user_id, first_name, last_name FROM user"

  addUser (NewUser fn ln) (SqlUserDal c) = do
    execute c "INSERT INTO user(first_name, last_name) VALUES (?, ?)" [fn, ln]
    UserId . fromIntegral <$> lastInsertRowId c
\end{code}
