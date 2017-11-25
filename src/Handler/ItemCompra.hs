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
    
getItemCompraWithIdR :: ItemCompraId -> Handler Value
getItemCompraWithIdR iid = do 
    itemCompra <- runDB $ get404 iid
    sendStatusJSON ok200 (object ["data" .= (toJSON itemCompra)])
    
putItemCompraWithIdR :: ItemCompraId -> Handler Value
putItemCompraWithIdR iid = do
    _ <- runDB $ get404 iid
    novoItemCompra <- requireJsonBody :: Handler ItemCompra
    runDB $ replace iid novoItemCompra
    sendStatusJSON noContent204 (object ["data" .= (iid)])
    
deleteItemCompraWithIdR :: ItemCompraId -> Handler Value
deleteItemCompraWithIdR iid = do 
    _ <- runDB $ get404 iid
    runDB $ delete iid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey iid)])