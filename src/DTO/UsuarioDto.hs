{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module DTO.UsuarioDto where

import Import
import Database.Persist.Postgresql

data UsuarioDto = UsuarioDto
    { login          :: Text
    , senha          :: Text
    , email          :: Text
    , tipo           :: Int
    , funcoes        :: [Entity Funcao]
    } deriving (Generic)
instance FromJSON UsuarioDto
instance ToJSON UsuarioDto