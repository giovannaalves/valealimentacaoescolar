{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Aluno where

import Import

import Database.Persist.Postgresql
import System.Random
import Handler.EnableCors
--import GHC.Generics
--import Data.Aeson
import qualified Data.Text as T (pack)

data AlunoDto = AlunoDto
    { nome           :: Text
    , limite         :: Double
    , cpfResponsavel :: Text
    , sexo           :: Text
    , status         :: Text
    , dataNascimento :: Text
    , login          :: Text
    , email          :: Text
      } deriving (Show, Generic)

instance FromJSON AlunoDto
instance ToJSON AlunoDto



getRandom :: IO Text
getRandom = do
    g <- getStdGen
    return $ T.pack $ take 10 (randomRs ('a','z') g) 

optionsAlunoR :: Handler Value
optionsAlunoR = optionGenerico "OPTIONS, GET, POST"

getAlunoR :: Handler Value
getAlunoR = do
    addCorsHeader "GET"
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON alunos)])

postAlunoR :: Handler Value
postAlunoR = do
    addCorsHeader "POST"
    alunoDto <- requireJsonBody :: Handler AlunoDto
    password <- liftIO $ getRandom
    let usuario = Usuario (nome alunoDto) password (email alunoDto) 4
    uid <- runDB $ insert usuario
    let aluno = Aluno (nome alunoDto) (limite alunoDto) (cpfResponsavel alunoDto) (sexo alunoDto) (status alunoDto) uid (dataNascimento alunoDto)  
    aid <- runDB $ insert aluno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aid)])

optionsAlunoWithIdR :: AlunoId -> Handler Value
optionsAlunoWithIdR _ = optionGenerico "OPTIONS, GET, PUT, DELETE"

getAlunoWithIdR :: AlunoId -> Handler Value
getAlunoWithIdR aid = do 
    addCorsHeader "GET"
    aluno <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["data" .= (toJSON aluno)])

putAlunoWithIdR :: AlunoId -> Handler Value
putAlunoWithIdR aid = do
    addCorsHeader "PUT"
    _ <- runDB $ get404 aid
    novoAluno <- requireJsonBody :: Handler Aluno
    runDB $ replace aid novoAluno
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])

deleteAlunoWithIdR :: AlunoId -> Handler Value
deleteAlunoWithIdR aid = do 
    addCorsHeader "DELETE"
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])

optionsSaldoR :: AlunoId -> Handler Value
optionsSaldoR _ = optionGenerico "OPTIONS, GET"

getValor :: ContaCorrente -> Double
getValor (ContaCorrente _ _ 1 s) = s
getValor (ContaCorrente _ _ _ s) = -s

getSaldoR :: AlunoId -> Handler Value
getSaldoR aid = do
    addCorsHeader "GET"
    operacoes <- runDB $ selectList [ContaCorrenteIdAluno ==. aid] []
    let result = sum $ map (getValor.(\(Entity _ o) -> o)) operacoes
    sendStatusJSON ok200 (object ["data" .= result])

optionsRecarregarR :: Handler Value
optionsRecarregarR = optionGenerico "OPTIONS, POST"

postRecarregarR :: Handler Value
postRecarregarR = do
    addCorsHeader "POST"
    conta <- requireJsonBody :: Handler ContaCorrente
    _ <- runDB $ insert conta
    sendStatusJSON noContent204 (emptyObject)