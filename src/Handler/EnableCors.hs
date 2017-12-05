{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.EnableCors where

import Import
import qualified Data.Text as T (pack)

addCorsHeader :: Text -> Handler ()
addCorsHeader metodos = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization, Content-Type"
    addHeader "Access-Control-Allow-Methods" metodos

optionGenerico :: String -> Handler Value
optionGenerico metodos = do
    addCorsHeader $ T.pack metodos
    sendStatusJSON ok200 emptyObject
