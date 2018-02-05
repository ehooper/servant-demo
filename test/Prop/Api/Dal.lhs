Specifying Properties
=====================

Typed (pure) functional programming allows us to express properties of our software with types. Here we have simple boolean propositions, but this infrastructure can be used to express sophisticated propositions and their proofs with theorem provers such as Coq.

\begin{code}
module Prop.Api.Dal where
\end{code}

<!--
\begin{code}
import Control.Monad (foldM)
import Data.List (nub)
import Api.Data
import Api.Dal
\end{code}
-->

Because of this, we can separate properties of our system into a list of predicates.

We use `user_in_user_list` to assert whether a user is in the user database. This is an `IO Bool` rather than just a `Bool` because we must interact with the database to get the answer.

\begin{code}
user_in_user_list :: (UserDal dal) => User -> dal -> IO Bool
user_in_user_list u dal = do
  result <- getUser (userId u) dal
  return $ case result of
             Just u' | u' == u -> True
             _ -> False
\end{code}

One property we want to hold for our DAL at all times is that all users returned from the `getAllUsers` function are, in fact, valid users in the database. We can use the `user_in_user_list` to assert each individual user is valid and use `foldM` to apply it over the user list.

\begin{code}
user_list_has_valid_users :: (UserDal dal) => dal -> IO Bool
user_list_has_valid_users dal = do
  users <- getAllUsers dal
  foldM (\rest u -> (&& rest) <$> user_in_user_list u dal) True users
\end{code}

After a user is added to the database, it should certainly be in the database.

\begin{code}
new_user_added :: (UserDal dal) => NewUser -> dal -> IO Bool
new_user_added nu@(NewUser fn ln) dal = do
  uid <- addUser nu dal
  user_in_user_list (User uid fn ln) dal
\end{code}

The set of user ids should always be unique. We can assert this by getting the list of all user ids and checking that removing duplicates does not change the list (using the expensive `nub` function which should be avoided in general).

\begin{code}
user_ids_unique :: (UserDal dal) => dal -> IO Bool
user_ids_unique dal = do
  userIds <- fmap userId <$> getAllUsers dal
  return $ userIds == nub userIds
\end{code}
