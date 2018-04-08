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
  (egid, _) <- requireAuthPair
  case egid of
    Left gid -> do
      mguest <- runDB (getBy (UniqueRsvp gid))
      let guest = case mguest of
            Just (Entity _ g)  -> g
            _ -> GuestRsvp gid True Nothing False 
      (formWidget, formEnctype) <- generateFormPost $ guestRsvpForm guest
      defaultLayout $(widgetFile "rsvp")
    _ -> defaultLayout $(widgetFile "invite")

postRsvpR :: Handler Html
postRsvpR = do
  defaultLayout $(widgetFile "invite")

yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]

guestRsvpForm :: GuestRsvp -> Form GuestRsvp
guestRsvpForm (GuestRsvp gid coming diet bus) = do
  let rsvp =
        GuestRsvp (gid) <$>
        areq
          yesNo
          ("Can you come" {fsAttrs = [("class", "attending-control")]})
          (Just coming) <*>
        aopt textField textSettings (Just diet) <*>
        areq
          yesNo
          ("Do you need the bus" {fsAttrs = [("class", "bus-control")]})
          (Just bus)
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