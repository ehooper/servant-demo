Testing the DAL
===============

<!--
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}
-->

We will use Hspec to define a test specification for both UserDal implementations.

\begin{code}
module DalTest where

import Api.Data
import Api.Dal
import Prop.Api.Dal
import Test.Hspec
\end{code}

\begin{code}
userDalSpec :: Spec
userDalSpec = do
  describe "Empty MockUserDal" $ before (initMockUserDal 0 []) userDalProperties
  describe "Nonempty MockUserDal" $ beforeAll (initMockUserDal 0 []) userDalActions
  sqlProvider <- runIO initSqlUserDal
  describe "Empty SqlUserDal" $ before (return sqlProvider) userDalProperties
  describe "Nonempty SqlUserDal" $ before (return sqlProvider) userDalActions

userDalProperties :: (UserDal dal) => SpecWith (DalProvider dal)
userDalProperties = do
  specify "all users in the user list are valid" $ \p ->
    withProvider p user_list_has_valid_users `shouldReturn` True

  specify "user ids are unique" $ \p ->
    withProvider p user_ids_unique `shouldReturn` True

userDalActions :: (UserDal dal) => SpecWith (DalProvider dal)
userDalActions = do
  it "adds a new user" $ \p ->
    withProvider p (new_user_added (NewUser "Test1" "Test")) `shouldReturn` True

  it "adds another new user" $ \p ->
    withProvider p (new_user_added (NewUser "Test2" "Test")) `shouldReturn` True

  it "has at least 2 users" $ \p ->
    ((>= 2) . length <$> withProvider p getAllUsers) `shouldReturn` True

  userDalProperties
\end{code}
