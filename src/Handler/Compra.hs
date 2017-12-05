{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Compra where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Handler.EnableCors

data CompraDto = CompraDto
    { idAluno :: AlunoId,
      total :: Double,
      dataCompra :: Text,
      itens :: [ItemCompra]}
      deriving (Generic)

instance FromJSON CompraDto
instance ToJSON CompraDto 

optionsCompraInsereR :: Handler Value
optionsCompraInsereR = optionGenerico "OPTIONS, GET, POST"

postCompraInsereR :: Handler Value
postCompraInsereR = do
    addCorsHeader "POST"
    compra <- requireJsonBody :: Handler Compra
    cid <- runDB $ insert compra
    contacorrente <- return $ ContaCorrente (compraIdAluno compra) "2017-12-02" 1 (compraTotal compra)
    _ <- runDB $ insert contacorrente
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])
    
optionsCompraWithIdR :: CompraId -> Handler Value
optionsCompraWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"

getCompraWithIdR :: CompraId -> Handler Value
getCompraWithIdR cid = do
    addCorsHeader "GET"
    compra <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON compra)])
    
putCompraWithIdR :: CompraId -> Handler Value
putCompraWithIdR cid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 cid
    novaCompra <- requireJsonBody :: Handler Compra
    runDB $ replace cid novaCompra
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])

deleteCompraWithIdR :: CompraId -> Handler Value
deleteCompraWithIdR cid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])
    
postComprarR :: AlunoId -> Handler Value
postComprarR aid = do
    addCorsHeader "POST"
    compraDto <- requireJsonBody :: Handler CompraDto
    let itensDaCompra = itens compraDto
    let total = sum $ map (\ic -> (itemCompraValorItem ic) * (fromIntegral (itemCompraQuantidade ic))) itensDaCompra
    let compra = Compra aid total (dataCompra compraDto)
    compraId <- runDB $ insert compra
    _ <- sequence $ map (runDB.insert) itensDaCompra
    sendStatusJSON ok200 (object ["data" .= (toJSON compraId)])