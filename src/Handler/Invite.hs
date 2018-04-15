{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Invite where

import Import
import Data.Guest
getInviteR :: Handler Html
getInviteR = do
  defaultLayout $(widgetFile "invite")

getInfoR :: Handler Html
getInfoR = do
  defaultLayout $(widgetFile "info")

getTravelR :: Handler Html
getTravelR = do
  defaultLayout $(widgetFile "getting-there")

getOnTheDayR :: Handler Html
getOnTheDayR = do
  defaultLayout $(widgetFile "on-the-day")
  
getAccommodationR :: Handler Html
getAccommodationR = do
  defaultLayout $(widgetFile "accommodation")


getRsvpR :: Handler Html
getRsvpR = do
  mguest <- getGuestId
  case mguest of
    Just guest -> do
      party <- wholePartyRsvp guest
      (formWidget, formEnctype) <- generateFormPost $ guestRsvpForm party
      defaultLayout $(widgetFile "rsvp")
    Nothing -> defaultLayout $(widgetFile "invite")

postRsvpR :: Handler Html
postRsvpR = do
  mguest <- getGuestId
  case mguest of
    Just guest -> do
      party <- wholePartyRsvp guest
      ((res, widget), enctype) <- runFormPost $ guestRsvpForm party
      case res of
        FormSuccess gs -> do
          runDB $ do
            traverse_ updateRsvp gs
        _ -> pure ()
      defaultLayout $(widgetFile "invite")
    _ -> defaultLayout $(widgetFile "invite")
  
updateRsvp :: GuestRsvp -> DB ()
updateRsvp g@(GuestRsvp gid _ _ _) = do 
  deleteBy (UniqueRsvp gid)
  insert_ g

getGuestId :: Handler (Maybe GuestId)
getGuestId = do
  (egid, _) <- requireAuthPair
  case egid of
    Left gid -> pure $ Just gid
    _ -> return Nothing

defaultRsvp gid = GuestRsvp gid True Nothing False
yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]

rsvpAForm :: GuestRsvp -> AForm (HandlerT App IO) GuestRsvp
rsvpAForm (GuestRsvp gid coming diet bus) = GuestRsvp (gid) <$>
            areq
              yesNo
              ("Can you come" {fsAttrs = [("class", "attending-control")]})
              (Just coming) <*>
            aopt textField textSettings (Just diet) <*>
            areq
              yesNo
              ("Do you need the bus" {fsAttrs = [("class", "bus-control")]})
              (Just bus)
  where
    textSettings =
      FieldSettings
      { fsLabel = "Do you have any dietary requirements?"
      , fsTooltip = Nothing
      , fsId = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "dietcontrol"), ("placeholder", "I hate peas")]
      }


guestRsvpForm :: [GuestRsvp] -> Form [GuestRsvp]
guestRsvpForm gs = do
  renderDivs (traverse rsvpAForm gs)

wholePartyRsvp :: GuestId -> Handler [GuestRsvp]
wholePartyRsvp gid = runDB $ do
  gs <- wholeParty' gid
  rsvps <- traverse (\i -> (maybe (defaultRsvp i) (\(Entity _ rsvp) -> rsvp)) <$>  (getBy $ UniqueRsvp i)) gs
  pure  rsvps

wholeParty' :: GuestId -> DB [GuestId]
wholeParty' g = do
    mpid <- get g  
    case mpid of 
      Just (Guest _ _ _ partyId) -> do 
          eguests <- lookupParty partyId
          pure $ (\(Entity k _) -> k) <$> eguests
      _ -> pure [g]

lookupParty :: Party -> DB [Entity Guest]
lookupParty partyId = selectList [GuestParty ==. partyId] []