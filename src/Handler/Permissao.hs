{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Permissao where

import Import
import Database.Persist.Postgresql
import Handler.EnableCors

optionsPermissaoByUsuarioR :: UsuarioId -> Handler Value
optionsPermissaoByUsuarioR _ = optionGenerico "OPTIONS, GET"

getPermissaoByUsuarioR :: UsuarioId -> Handler Value
getPermissaoByUsuarioR rid = do 
    addCorsHeader "GET"
    permissoes' <- runDB $ selectList [PermissaoIdUsuario ==. rid] []
    let permissoes = fmap (\(Entity _ alo) -> alo) permissoes'
    let pids = fmap permissaoIdFuncao permissoes 
    funcoes <- sequence $ fmap (\pid -> runDB $ get404 pid) pids
    sendStatusJSON ok200 (object ["data" .= (toJSON funcoes)])


optionsPermissaoR :: UsuarioId -> FuncaoId -> Handler Value
optionsPermissaoR _ _ = optionGenerico "OPTIONS, POST, DELETE"

postPermissaoR :: UsuarioId -> FuncaoId -> Handler Value
postPermissaoR usuarioid funcaoid = do
    addCorsHeader "POST"
    permissao <- return $ Permissao usuarioid funcaoid
    rid <- runDB $ insert permissao
    sendStatusJSON created201 (object ["data" .= (fromSqlKey rid)])

--  UniquePermissao         idUsuario idFuncao
deletePermissaoR :: UsuarioId -> FuncaoId -> Handler Value
deletePermissaoR aid pid = do 
    addCorsHeader "DELETE"
    runDB $ deleteBy $ UniquePermissao aid pid
    sendStatusJSON noContent204 emptyObject