{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Restricao where

import Import
import Database.Persist.Postgresql

postRestricaoInsereR :: Handler Value
postRestricaoInsereR = do
    restricao <- requireJsonBody :: Handler Restricao
    rid <- runDB $ insert restricao
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])

getRestricaoByAlunoR :: AlunoId -> Handler Value
getRestricaoByAlunoR rid = do 
    restricoes' <- runDB $ selectList [RestricaoIdAluno ==. rid] []
    restricoes <- return $ fmap (\(Entity _ alo) -> alo) restricoes'
    pids <- return $ fmap restricaoIdProduto restricoes 
    produtos <- sequence $ fmap (\pid -> runDB $ get404 pid) pids
    sendStatusJSON ok200 (object ["data" .= (toJSON produtos)])