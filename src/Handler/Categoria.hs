{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Categoria where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postCategoriaInsereR :: Handler Value
postCategoriaInsereR = do
    categoria <- requireJsonBody :: Handler Categoria
    cid <- runDB $ insert categoria
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])
    
getCategoriaWithIdR :: CategoriaId -> Handler Value
getCategoriaWithIdR cid = do 
    categoria <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON categoria)])

deleteCategoriaWithIdR :: CategoriaId -> Handler Value
deleteCategoriaWithIdR cid = do 
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])

putCategoriaWithIdR :: CategoriaId -> Handler Value
putCategoriaWithIdR cid = do
    _ <- runDB $ get404 cid
    novaCategoria <- requireJsonBody :: Handler Categoria
    runDB $ replace cid novaCategoria
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)]) 