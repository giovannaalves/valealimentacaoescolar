{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postProdutoInsereR :: Handler Value
postProdutoInsereR = do
    produto <- requireJsonBody :: Handler Produto
    pid <- runDB $ insert produto
    sendStatusJSON created201 (object ["data" .= (fromSqlKey pid)]
    
getProdutoWithIdR :: ProdutoId -> Handler Value
getProdutoWithIdR pid = do 
    produto <- runDB $ get404 pid
    sendStatusJSON ok200 (object ["data" .= (toJSON produto)])

putProdutoWithIdR :: ProdutoId -> Handler Value
putProdutoWithIdR pid = do
    _ <- runDB $ get404 pid
    novoProduto <- requireJsonBody :: Handler Produto
    runDB $ replace pid novoProduto
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey pid)])
    
deleteProdutoWithIdR :: ProdutoId -> Handler Value
deleteProdutoWithIdR pid = do 
    _ <- runDB $ get404 pid
    runDB $ delete pid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey pid)])