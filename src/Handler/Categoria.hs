{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Categoria where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postCategoriaInsereR :: Handler Value
postCategoriaInsereR = do
    categoria <- requireJsonBody :: Handler Categoria
    cid <- runDB $ insert categoria
    sendStatusJSON created201 (object ["data" .= (fromSqlKey cid)])