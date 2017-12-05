{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql
import Handler.EnableCors

optionsUsuarioR :: Handler Value
optionsUsuarioR = optionGenerico "OPTIONS, GET, POST"

optionsUsuarioWithIdR :: UsuarioId -> Handler Value
optionsUsuarioWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"

getUsuarioR :: Handler Value
getUsuarioR = do
    addCorsHeader "GET"
    usuarios <- runDB $ selectList [] [Asc UsuarioLogin]
    sendStatusJSON ok200 (object ["data" .= (toJSON usuarios)])

postUsuarioR :: Handler Value
postUsuarioR = do
    addCorsHeader "POST"
    usuario <- requireJsonBody :: Handler Usuario
    uid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["data" .= (fromSqlKey uid)])

getUsuarioWithIdR :: UsuarioId -> Handler Value
getUsuarioWithIdR uid = do 
    addCorsHeader "GET"
    usuario <- runDB $ get404 uid
    sendStatusJSON ok200 (object ["data" .= (toJSON usuario)])
    
deleteUsuarioWithIdR:: UsuarioId -> Handler Value
deleteUsuarioWithIdR uid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 uid
    runDB $ delete uid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey uid)])

putUsuarioWithIdR:: UsuarioId -> Handler Value
putUsuarioWithIdR uid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 uid
    novoUsuario<- requireJsonBody :: Handler Usuario
    runDB $ replace uid novoUsuario
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey uid)])