{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.AlergenoAluno where

import Import
import Database.Persist.Postgresql

postAlergenoAlunoInsereR :: Handler Value
postAlergenoAlunoInsereR = do
    alergenoaluno <- requireJsonBody :: Handler AlergenoAluno
    aaid <- runDB $ insert alergenoaluno
    sendStatusJSON created201 (object ["data" .= (fromSqlKey aaid)])
