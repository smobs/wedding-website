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

getGuestsR :: Handler Html
getGuestsR =  do
    guests <- runDB $ selectList [] []
    defaultLayout $(widgetFile "guests")
getGuestR :: Key Guest -> Handler Html
getGuestR _ =  defaultLayout  $(widgetFile "info")
postGuestR :: Key Guest -> Handler Html
postGuestR _ =  defaultLayout  $(widgetFile "info")