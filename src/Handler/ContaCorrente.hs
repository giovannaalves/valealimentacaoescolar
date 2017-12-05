{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ContaCorrente where

import Import
import Database.Persist.Postgresql
import Handler.EnableCors

optionsContaCorrenteR :: Handler Value
optionsContaCorrenteR = optionGenerico "OPTIONS, POST"

postContaCorrenteR :: Handler Value
postContaCorrenteR = do
    addCorsHeader "POST"
    contacorrente <- requireJsonBody :: Handler ContaCorrente
    cid <- runDB $ insert contacorrente
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])

optionsContaCorrenteWithIdR :: ContaCorrenteId -> Handler Value
optionsContaCorrenteWithIdR _ = optionGenerico "OPTIONS, GET, DELETE"

getContaCorrenteWithIdR :: ContaCorrenteId -> Handler Value
getContaCorrenteWithIdR cid = do
    addCorsHeader "GET"
    contacorrente <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["data" .= (toJSON contacorrente)])
    
deleteContaCorrenteWithIdR :: ContaCorrenteId -> Handler Value
deleteContaCorrenteWithIdR cid = do
    addCorsHeader "DELETE"
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey cid)])
