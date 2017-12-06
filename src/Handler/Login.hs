{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Login where

import Import
import Database.Persist.Postgresql
import Handler.EnableCors
import qualified DTO.UsuarioDto as User

data LoginDto = LoginDto
    { login          :: Text
    , senha          :: Text
    } deriving (Show, Generic)
instance FromJSON LoginDto
instance ToJSON LoginDto

data LoginResult = LoginResult 
    { usuario :: User.UsuarioDto
    , dados   :: Value
    } deriving (Generic)
instance FromJSON LoginResult
instance ToJSON LoginResult

optionsLoginVAER :: Handler Value
optionsLoginVAER = optionGenerico "OPTIONS, POST"


postLoginVAER :: Handler Value
postLoginVAER = do
    addCorsHeader "POST"
    credencial <- requireJsonBody :: Handler LoginDto
    usuario <- runDB $ getBy $ UniqueLogin (login credencial) (senha credencial)
    case usuario of
        Nothing -> sendStatusJSON ok200 (object ["data" .= emptyObject])
        (Just (Entity usuarioId (Usuario user password email tipo))) -> do
            
           {- permissoes' <- runDB $ selectList [PermissaoIdUsuario ==. usuarioId] []
            let permissoes = fmap (\(Entity _ alo) -> alo) permissoes'
            let fids = fmap permissaoIdFuncao permissoes 
            let funcoes = []
            
            let userResult = User.UsuarioDto user password email tipo funcoes
            dados <- case tipo of
                    3 -> do
                        responsavel <- getBy $ UniqueUserResponsavel usuarioId
                        toJSON responsavel
                        --sendStatusJSON ok200 (object ["data" .= toJSON $ LoginResult userResult $ toJSON responsavel])
                    4 -> do
                        aluno <- getBy $ UniqueUserAluno usuarioId
                        toJSON aluno
                        --sendStatusJSON ok200 (object ["data" .= toJSON $ LoginResult userResult $ toJSON alunol])
                    _ -> Null-}
            sendStatusJSON ok200 (object ["data" .= object [
                "usuario" .= toJSON usuario
                --,
                --"dados" .= dados
                ]])
                
            