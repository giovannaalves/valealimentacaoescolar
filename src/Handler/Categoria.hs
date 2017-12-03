{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Categoria where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Handler.EnableCors

optionsCategoriaR :: Handler Value
optionsCategoriaR = optionGenerico "OPTIONS, GET, POST"

getCategoriaR :: Handler Value
getCategoriaR = do
    addCorsHeader "GET"
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON categorias)])

postCategoriaR :: Handler Value
postCategoriaR = do
    addCorsHeader "POST"
    categoria <- requireJsonBody :: Handler Categoria
    cid <- runDB $ insert categoria
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])

optionsCategoriaWithIdR :: CategoriaId -> Handler Value
optionsCategoriaWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"
    
getCategoriaWithIdR :: CategoriaId -> Handler Value
getCategoriaWithIdR cid = do 
    addCorsHeader "GET"
    categoria <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON categoria)])

deleteCategoriaWithIdR :: CategoriaId -> Handler Value
deleteCategoriaWithIdR cid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])

putCategoriaWithIdR :: CategoriaId -> Handler Value
putCategoriaWithIdR cid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 cid
    novaCategoria <- requireJsonBody :: Handler Categoria
    runDB $ replace cid novaCategoria
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)]) 