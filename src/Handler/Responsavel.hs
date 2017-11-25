{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Responsavel where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql


--insert into responsavel values (...)
postResponsavelInsereR :: Handler Value
postResponsavelInsereR = do
    responsavel <- requireJsonBody :: Handler Responsavel
    rid <- runDB $ insert responsavel
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])
-- #EXAMPLE: curl -v -X POST https://haskdelta-romefeller.c9users.io/responsavel -d '{"nome":"Mari", "cpf":"488258966333", "email":"mari.gijv@live.com", "celular":"981415285","telefone":"33252525","complemento":"casa", "numeroend":1,"cep":"11225555"}'

-- select * from Responsavel where id = rid
getResponsavelWithIdR :: ResponsavelId -> Handler Value
getResponsavelWithIdR rid = do 
    responsavel <- runDB $ get404 rid
    sendStatusJSON ok200 (object ["data" .= (toJSON responsavel)])
    
    
putResponsavelWithIdR :: ResponsavelId -> Handler Value
putResponsavelWithIdR rid = do
    _ <- runDB $ get404 rid
    novoResponsavel <- requireJsonBody :: Handler Responsavel
    runDB $ replace rid novoResponsavel
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey rid)])
    