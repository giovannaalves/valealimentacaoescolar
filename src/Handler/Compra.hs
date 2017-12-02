{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Compra where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time.Clock
import Data.Time.Calendar

postCompraInsereR :: Handler Value
postCompraInsereR = do
    compra <- requireJsonBody :: Handler Compra
    cid <- runDB $ insert compra
    contacorrente <- return $ ContaCorrente (compraIdAluno compra) "2017-12-02" 1 (compraTotal compra)
    _ <- runDB $ insert contacorrente
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])

getCompraWithIdR :: CompraId -> Handler Value
getCompraWithIdR cid = do 
    compra <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON compra)])
    
putCompraWithIdR :: CompraId -> Handler Value
putCompraWithIdR cid = do
    _ <- runDB $ get404 cid
    novaCompra <- requireJsonBody :: Handler Compra
    runDB $ replace cid novaCompra
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])

deleteCompraWithIdR :: CompraId -> Handler Value
deleteCompraWithIdR cid = do 
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])