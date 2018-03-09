{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
  ( Handle
  , ChanMessage (..)
  , registeredUsers
  , messageChannel
  , initialize
  ) where

import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Data.Map.Strict as Map
import qualified Model.Markdown as MD
import qualified Model.User as U

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { registeredUsers :: STM.TVar U.Users
  , messageChannel  :: STM.TChan ChanMessage
  }


initialize :: IO Handle 
initialize = do
  regUsers <- STM.newTVarIO (U.Users Map.empty Map.empty)
  chan <- STM.newBroadcastTChanIO
  return $ Handle regUsers chan


data ChanMessage
  = Broadcast U.UserName MD.Markdown
  | Whisper U.UserId U.UserName MD.Markdown
