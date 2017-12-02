{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Responsavel where

import Import
import Database.Persist.Postgresql
import System.Random
import qualified Data.Text as T (pack)
import Handler.EnableCors
-- tipo generico para o aeson
import GHC.Generics
--transformar o json em responsavel dto
import Data.Aeson

getRandom :: IO Text
getRandom = do
    g <- getStdGen
    return $ T.pack $ take 10 (randomRs ('a','z') g) 

data ResponsavelDto = ResponsavelDto
    { nome           :: Text
    , cpfResponsavel :: Text
    , celular        :: Text
    , telefone       :: Text
    , complemento    :: Text
    , numeroCasa     :: Int
    , cep            :: Text
    , sexo           :: Text
    , login          :: Text
    , email          :: Text
      } deriving (Show, Generic)

instance FromJSON ResponsavelDto
instance ToJSON ResponsavelDto

optionsResponsavelR :: Handler Value
optionsResponsavelR = optionGenerico "OPTIONS, GET, POST"


getResponsavelR :: Handler Value
getResponsavelR = do
    addCorsHeader "GET"
    responsaveis <- runDB $ selectList [] [Asc ResponsavelNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON responsaveis)])

--insert into responsavel values (...)
postResponsavelR :: Handler Value
postResponsavelR = do
    addCorsHeader "POST"
    responsavelDto <- requireJsonBody :: Handler ResponsavelDto
    password <- liftIO $ getRandom
    let usuario = Usuario (nome responsavelDto) password (email responsavelDto) 3
    uid <- runDB $ insert usuario
    let responsavel = Responsavel (nome responsavelDto) (cpfResponsavel responsavelDto) (celular responsavelDto) (telefone responsavelDto) (complemento responsavelDto) (numeroCasa responsavelDto) (cep responsavelDto) uid (sexo responsavelDto)
    rid <- runDB $ insert responsavel
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])
-- #EXAMPLE: curl -v -X POST https://haskdelta-romefeller.c9users.io/responsavel -d '{"nome":"Mari", "cpf":"488258966333", "email":"mari.gijv@live.com", "celular":"981415285","telefone":"33252525","complemento":"casa", "numeroend":1,"cep":"11225555"}'


optionsResponsavelWithIdR :: ResponsavelId -> Handler Value
optionsResponsavelWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"


-- select * from Responsavel where id = rid
getResponsavelWithIdR :: ResponsavelId -> Handler Value
getResponsavelWithIdR rid = do 
    addCorsHeader "GET"
    responsavel <- runDB $ get404 rid
    sendStatusJSON ok200 (object ["data" .= (toJSON responsavel)])
    
    
putResponsavelWithIdR :: ResponsavelId -> Handler Value
putResponsavelWithIdR rid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 rid
    novoResponsavel <- requireJsonBody :: Handler Responsavel
    runDB $ replace rid novoResponsavel
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey rid)])

deleteResponsavelWithIdR :: ResponsavelId -> Handler Value
deleteResponsavelWithIdR rid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 rid
    runDB $ delete rid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey rid)])