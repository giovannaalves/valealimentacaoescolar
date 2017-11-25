{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
module Handler.AlergenoProduto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

postAlergenoAlunoInsereR :: Handler Value
postAlergenoAlunoInsereR = do
    alergenoproduto <- requireJsonBody :: Handler AlergenoProduto
    apid <- runDB $ insert alergenoproduto
    sendStatusJSON created201 (object ["data" .= (fromSqlKey apid)])