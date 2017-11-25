{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Restricao where

import Import
import Database.Persist.Postgresql

postRestricaoInsereR :: Handler Value
postRestricaoInsereR = do
    restricao <- requireJsonBody :: Handler Restricao
    rid <- runDB $ insert restricao
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])

