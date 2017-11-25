{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postProdutoInsereR :: Handler Value
postProdutoInsereR = do
    produto <- requireJsonBody :: Handler Produto
    pid <- runDB $ insert produto
    sendStatusJSON created201 (object ["data" .= (fromSqlKey pid)]