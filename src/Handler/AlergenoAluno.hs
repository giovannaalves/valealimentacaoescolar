{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AlergenoAluno where

import Import
import Database.Persist.Postgresql

postAlergenoAlunoR :: AlunoId -> AlergenoId -> Handler Value
postAlergenoAlunoR alunoid alergenoid = do
    alergenoaluno <- return $ AlergenoAluno alunoid alergenoid
    aaid <- runDB $ insert alergenoaluno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aaid)])

getAlergenoByAlunoR :: AlunoId -> Handler Value
getAlergenoByAlunoR aaid = do 
    alergenos' <- runDB $ selectList [AlergenoAlunoIdAluno ==. aaid] []
    alergenos <- return $ fmap (\(Entity _ alo) -> alo) alergenos'
    aids <- return $ fmap alergenoAlunoIdAlergeno alergenos 
    alergenos <- sequence $ fmap (\aid -> runDB $ get404 aid) aids
    sendStatusJSON ok200 (object ["data" .= (toJSON alergenos)])

deleteAlergenoAlunoR :: AlunoId -> AlergenoId -> Handler Value
deleteAlergenoAlunoR alunoid alergenoid = do 
    runDB $ deleteBy $ UniqueAlergenoAluno alunoid alergenoid
    sendStatusJSON noContent204 emptyObject
