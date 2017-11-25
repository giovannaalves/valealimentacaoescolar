{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Alergeno where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql

-- $ curl -v -X POST https://haskdelta-romefeller.alergeno -d '{"nome":"Giovanna","limite":22.5,"cpf":"488258966333", "respid":1}'
-- insert into Alergeno values ('lactose')
postAlergenoInsereR :: Handler Value
postAlergenoInsereR = do
    alergeno <- requireJsonBody :: Handler Alergeno
    aid <- runDB $ insert alergeno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aid)])
    
getAlergenoWithIdR :: AlergenoId -> Handler Value
getAlergenoWithIdR aid = do 
    alergeno <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["data" .= (toJSON alergeno)])
    
putAlergenoWithIdR :: AlergenoId -> Handler Value
putAlergenoWithIdR aid = do
    _ <- runDB $ get404 aid
    novoAlergeno <- requireJsonBody :: Handler Alergeno
    runDB $ replace aid novoAlergeno
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])
    
deleteAlergenoWithIdR  :: AlergenoId -> Handler Value
deleteAlergenoWithIdR aid = do 
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])
