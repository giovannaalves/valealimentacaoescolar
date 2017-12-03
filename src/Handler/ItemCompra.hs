{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ItemCompra where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Handler.EnableCors

optionsItemCompraInsereR :: Handler Value
optionsItemCompraInsereR = optionGenerico "OPTIONS, GET, POST"

postItemCompraInsereR :: Handler Value
postItemCompraInsereR = do
    addCorsHeader "POST"
    itemCompra <- requireJsonBody :: Handler Compra
    iid <- runDB $ insert itemCompra
    sendStatusJSON created201 (object ["data" .= (fromSqlKey iid)])

optionsItemCompraWithIdR :: ItemCompraId -> Handler Value
optionsItemCompraWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"

getItemCompraWithIdR :: ItemCompraId -> Handler Value
getItemCompraWithIdR iid = do
    addCorsHeader "GET"
    itemCompra <- runDB $ get404 iid
    sendStatusJSON ok200 (object ["data" .= (toJSON itemCompra)])
    
putItemCompraWithIdR :: ItemCompraId -> Handler Value
putItemCompraWithIdR iid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 iid
    novoItemCompra <- requireJsonBody :: Handler ItemCompra
    runDB $ replace iid novoItemCompra
    sendStatusJSON noContent204 (object ["data" .= (iid)])
    
deleteItemCompraWithIdR :: ItemCompraId -> Handler Value
deleteItemCompraWithIdR iid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 iid
    runDB $ delete iid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey iid)])