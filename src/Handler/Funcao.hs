{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Funcao where

import Import
import Database.Persist.Postgresql
import Handler.EnableCors

optionsFuncaoR :: Handler Value
optionsFuncaoR = optionGenerico "OPTIONS, GET, POST"

getFuncaoR :: Handler Value
getFuncaoR = do
    addCorsHeader "GET"
    funcoes <- runDB $ selectList [] [Asc FuncaoNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON funcoes)])

postFuncaoR :: Handler Value
postFuncaoR = do
    addCorsHeader "POST"
    funcao <- requireJsonBody :: Handler Funcao
    pid <- runDB $ insert funcao
    sendStatusJSON created201 (object ["data" .= (fromSqlKey pid)])

optionsFuncaoWithIdR :: FuncaoId -> Handler Value
optionsFuncaoWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"
    
getFuncaoWithIdR :: FuncaoId -> Handler Value
getFuncaoWithIdR pid = do
    addCorsHeader "GET"
    funcao <- runDB $ get404 pid
    sendStatusJSON ok200 (object ["data" .= (toJSON funcao)])
    
putFuncaoWithIdR :: FuncaoId -> Handler Value
putFuncaoWithIdR pid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 pid
    novoFuncao <- requireJsonBody :: Handler Funcao
    runDB $ replace pid novoFuncao
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey pid)])
    
deleteFuncaoWithIdR  :: FuncaoId -> Handler Value
deleteFuncaoWithIdR pid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 pid
    runDB $ delete pid
    sendStatusJSON noContent204 emptyObject
