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
    
getCategoriaBuscarR :: CategoriaId -> Handler Value
getCategoriaBuscarR cid = do 
    categoria <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON categoria)])

deleteCategoriaApagarR :: CategoriaId -> Handler Value
deleteCategoriaApagarR cid = do 
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])
