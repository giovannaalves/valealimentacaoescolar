{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Usario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postUsuarioInsereR :: Handler Value
postUsuarioInsereR = do
    usuario <- requireJsonBody :: Handler Usuario
    uid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["data" .= (fromSqlKey uid)])
