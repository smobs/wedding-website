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
  mguest <- getUserRsvp
  case mguest of
    Just guest -> do
      (formWidget, formEnctype) <- generateFormPost $ guestRsvpForm guest
      defaultLayout $(widgetFile "rsvp")
    Nothing -> defaultLayout $(widgetFile "invite")

postRsvpR :: Handler Html
postRsvpR = do
  mguest <- getUserRsvp
  case mguest of
    Just guest -> do
      ((res, widget), enctype) <- runFormPost $ guestRsvpForm guest
      case res of
        FormSuccess g@(GuestRsvp gid _ _ _) -> do
          runDB $ do 
            deleteBy (UniqueRsvp gid)
            insert_ g 
        _ -> pure ()
      defaultLayout $(widgetFile "invite")
    _ -> defaultLayout $(widgetFile "invite")

getUserRsvp :: Handler (Maybe GuestRsvp)
getUserRsvp = do
  (egid, _) <- requireAuthPair
  case egid of
    Left gid -> do
      mguest <- runDB (getBy (UniqueRsvp gid))
      let guest =
            case mguest of
              Just (Entity _ g) -> g
              _ -> GuestRsvp gid True Nothing False
      return (Just guest)
    _ -> return Nothing

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
