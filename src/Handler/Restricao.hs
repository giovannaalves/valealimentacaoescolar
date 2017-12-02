{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Restricao where

import Import
import Database.Persist.Postgresql

postRestricaoR :: AlunoId -> ProdutoId -> Handler Value
postRestricaoR alunoid produtoid = do
    restricao <- return $ Restricao alunoid produtoid
    rid <- runDB $ insert restricao
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])

getRestricaoByAlunoR :: AlunoId -> Handler Value
getRestricaoByAlunoR rid = do 
    restricoes' <- runDB $ selectList [RestricaoIdAluno ==. rid] []
    restricoes <- return $ fmap (\(Entity _ alo) -> alo) restricoes'
    pids <- return $ fmap restricaoIdProduto restricoes 
    produtos <- sequence $ fmap (\pid -> runDB $ get404 pid) pids
    sendStatusJSON ok200 (object ["data" .= (toJSON produtos)])

--  UniqueRestricao         idAluno idProduto
deleteRestricaoR :: AlunoId -> ProdutoId -> Handler Value
deleteRestricaoR aid pid = do 
    runDB $ deleteBy $ UniqueRestricao aid pid
    sendStatusJSON noContent204 emptyObject