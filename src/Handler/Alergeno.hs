{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Alergeno where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Handler.EnableCors

optionsAlergenoR :: Handler Value
optionsAlergenoR = optionGenerico "OPTIONS, GET, POST"

-- $ curl -v -X POST https://haskdelta-romefeller.alergeno -d '{"nome":"Giovanna","limite":22.5,"cpf":"488258966333", "respid":1}'
-- insert into Alergeno values ('lactose')
getAlergenoR :: Handler Value
getAlergenoR = do
    addCorsHeader "GET"
    alergenos <- runDB $ selectList [] [Asc AlergenoNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON alergenos)])

postAlergenoR :: Handler Value
postAlergenoR = do
    addCorsHeader "POST"
    alergeno <- requireJsonBody :: Handler Alergeno
    aid <- runDB $ insert alergeno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aid)])

optionsAlergenoWithIdR :: AlergenoId -> Handler Value
optionsAlergenoWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"
    
getAlergenoWithIdR :: AlergenoId -> Handler Value
getAlergenoWithIdR aid = do
    addCorsHeader "GET"
    alergeno <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["data" .= (toJSON alergeno)])
    
putAlergenoWithIdR :: AlergenoId -> Handler Value
putAlergenoWithIdR aid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 aid
    novoAlergeno <- requireJsonBody :: Handler Alergeno
    runDB $ replace aid novoAlergeno
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])
    
deleteAlergenoWithIdR  :: AlergenoId -> Handler Value
deleteAlergenoWithIdR aid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])
