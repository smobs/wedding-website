{-# LANGUAGE TemplateHaskell #-}
module Handler.Invite where

import Import

getInviteR :: Handler Html
getInviteR = do 
    defaultLayout
        $(widgetFile "invite")
getInfoR :: Handler Html
getInfoR = do 
    defaultLayout
        $(widgetFile "info")

getRsvpR :: Handler Html
getRsvpR = do 
    defaultLayout
        $(widgetFile "invite")

postRsvpR :: Handler Html
postRsvpR =  do 
    defaultLayout
        $(widgetFile "invite")

