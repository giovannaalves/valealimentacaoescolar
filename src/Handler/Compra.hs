{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Compra where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postCompraInsereR :: Handler Value
postCompraInsereR = do
    compra <- requireJsonBody :: Handler Compra
    cid <- runDB $ insert compra
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])