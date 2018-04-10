{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Auth.SecretHardcoded

getAdminLoginR :: Handler Html
getAdminLoginR = do 
    defaultLayout $
        secretLoginWidget AuthR