{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ContaCorrente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postContaCorrenteInsereR :: Handler Value
postContaCorrenteInsereR = do
    contacorrente <- requireJsonBody :: Handler ContaCorrente
    cid <- runDB $ insert contacorrente
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)]) 