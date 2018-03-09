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
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import qualified Data.Map.Strict as Map
import qualified Model.User as U
import qualified Channel as Ch

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { registeredUsers  :: STM.TVar U.Users
  , broadcastChannel :: Ch.Handle
  }


initialize :: IO Handle
initialize = do
  regUsers <- STM.newTVarIO (U.Users Map.empty Map.empty)
  chan <- Ch.initialize
  return $ Handle regUsers chan


useChannel :: Handle -> (Ch.Handle -> a) -> a
useChannel handle f = f (broadcastChannel handle)
