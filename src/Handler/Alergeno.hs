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