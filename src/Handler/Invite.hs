{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Invite where

import Import

getInviteR :: Handler Html
getInviteR = do
  defaultLayout $(widgetFile "invite")

getInfoR :: Handler Html
getInfoR = do
  defaultLayout $(widgetFile "info")

getRsvpR :: Handler Html
getRsvpR = do
  _ <- requireAuthPair
  (formWidget, formEnctype) <- generateFormPost guestRsvpForm
  
  defaultLayout $(widgetFile "rsvp")

postRsvpR :: Handler Html
postRsvpR = do
  defaultLayout $(widgetFile "invite")

yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]

guestRsvpForm :: Form GuestRsvp
guestRsvpForm = do
  let rsvp = GuestRsvp <$> areq yesNo ("Can you come" {fsAttrs = [("class", "attending-control")]}) (Just True) 
                   <*> areq textField textSettings Nothing 
                   <*> areq yesNo ("Do you need the bus" {fsAttrs = [("class", "bus-control")]}) (Just True) 
  
  renderDivs rsvp
  where
    textSettings =
      FieldSettings
      { fsLabel = "Do you have any dietary requirements?"
      , fsTooltip = Nothing
      , fsId = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "dietcontrol"), ("placeholder", "I hate peas")]
      }

