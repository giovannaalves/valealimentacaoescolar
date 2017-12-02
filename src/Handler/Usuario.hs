{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getUsuarioR :: Handler Value
getUsuarioR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioLogin]
    sendStatusJSON ok200 (object ["data" .= (toJSON usuarios)])

postUsuarioR :: Handler Value
postUsuarioR = do
    usuario <- requireJsonBody :: Handler Usuario
    uid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["data" .= (fromSqlKey uid)])

getUsuarioWithIdR :: UsuarioId -> Handler Value
getUsuarioWithIdR uid = do 
    usuario <- runDB $ get404 uid
    sendStatusJSON ok200 (object ["data" .= (toJSON usuario)])
    
deleteUsuarioWithIdR:: UsuarioId -> Handler Value
deleteUsuarioWithIdR uid = do 
    _ <- runDB $ get404 uid
    runDB $ delete uid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey uid)])

putUsuarioWithIdR:: UsuarioId -> Handler Value
putUsuarioWithIdR uid = do
    _ <- runDB $ get404 uid
    novoUsuario<- requireJsonBody :: Handler Usuario
    runDB $ replace uid novoUsuario
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey uid)])