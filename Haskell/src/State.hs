{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
  ( Handle
  , registeredUsers
  , initialize
  , useChannel
  , useUsers
  ) where

import qualified Channel as Ch
import qualified Users as Us

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { registeredUsers  :: Us.Handle
  , broadcastChannel :: Ch.Handle
  }


initialize :: IO Handle
initialize = do
  regUsers <- Us.initialize
  chan <- Ch.initialize
  return $ Handle regUsers chan


useChannel :: Handle -> (Ch.Handle -> a) -> a
useChannel handle f = f (broadcastChannel handle)


useUsers :: Handle -> (Us.Handle -> a) -> a
useUsers handle f = f (registeredUsers handle)
