{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Aluno where

import Import

import Database.Persist.Postgresql

getAlunoR :: Handler Value
getAlunoR = do
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    sendStatusJSON ok200 (object ["data" .= (toJSON alunos)])

postAlunoR :: Handler Value
postAlunoR = do
    aluno <- requireJsonBody :: Handler Aluno
    aid <- runDB $ insert aluno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aid)])

getAlunoWithIdR :: AlunoId -> Handler Value
getAlunoWithIdR aid = do 
    aluno <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["data" .= (toJSON aluno)])
    
putAlunoWithIdR :: AlunoId -> Handler Value
putAlunoWithIdR aid = do
    _ <- runDB $ get404 aid
    novoAluno <- requireJsonBody :: Handler Aluno
    runDB $ replace aid novoAluno
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])

deleteAlunoWithIdR :: AlunoId -> Handler Value
deleteAlunoWithIdR aid = do 
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["data" .= (fromSqlKey aid)])