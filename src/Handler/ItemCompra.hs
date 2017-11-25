{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ItemCompra where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postItemCompraInsereR :: Handler Value
postItemCompraInsereR = do
    itemCompra <- requireJsonBody :: Handler Compra
    iid <- runDB $ insert itemCompra
    sendStatusJSON created201 (object ["data" .= (fromSqlKey iid)])