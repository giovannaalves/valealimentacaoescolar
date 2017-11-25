{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.AlergenoProduto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postAlergenoProdutoInsereR :: Handler Value
postAlergenoProdutoInsereR = do
    alergenoproduto <- requireJsonBody :: Handler AlergenoProduto
    apid <- runDB $ insert alergenoproduto
    sendStatusJSON created201 (object ["data" .= (fromSqlKey apid)])
    
getAlergenoProdutoAWithIdR :: AlergenoProdutoId -> Handler Value
getAlergenoProdutoAWithIdR aid = do 
    alergeno <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["data" .= (toJSON alergeno)])