{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.AlergenoProduto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postAlergenoProdutoR :: ProdutoId -> AlergenoId -> Handler Value
postAlergenoProdutoR produtoid alergenoid = do
    alergenoproduto <- return $ AlergenoProduto produtoid alergenoid
    ppid <- runDB $ insert alergenoproduto
    sendStatusJSON created201 (object ["data" .= (fromSqlKey ppid)])

getAlergenoByProdutoR :: ProdutoId -> Handler Value
getAlergenoByProdutoR ppid = do 
    alergenos' <- runDB $ selectList [AlergenoProdutoProdid ==. ppid] []
    alergenos <- return $ fmap (\(Entity _ alo) -> alo) alergenos'
    pids <- return $ fmap alergenoProdutoAlergid alergenos 
    alergenos <- sequence $ fmap (\pid -> runDB $ get404 pid) pids
    sendStatusJSON ok200 (object ["data" .= (toJSON alergenos)])

deleteAlergenoProdutoR :: ProdutoId -> AlergenoId -> Handler Value
deleteAlergenoProdutoR pid aid = do 
    runDB $ deleteBy $ UniqueAlergenoProduto pid aid
    sendStatusJSON noContent204 emptyObject