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
    (formWidget, formEnctype) <- generateFormPost guestRsvpForm
    defaultLayout $(widgetFile "rsvp")

postRsvpR :: Handler Html
postRsvpR = do
  defaultLayout $(widgetFile "invite")

guestRsvpForm :: Form GuestRsvp
guestRsvpForm =
  renderDivs $
  GuestRsvp <$> (areq yesNo textSettings (Just False)) <*>
  (areq textField textSettings Nothing) <*>
  (areq yesNo textSettings (Just False))
  where
    textSettings =
      FieldSettings
      { fsLabel = "Dietary requirements"
      , fsTooltip = Nothing
      , fsId = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "I hate peas")]
      }
    yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]
