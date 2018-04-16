{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Invite where

import Import
import Data.Guest
import Yesod

type NamedRsvp = (GuestRsvp)

data PartyRsvp = Solo NamedRsvp | Couple NamedRsvp NamedRsvp


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
  guest <- getGuestId
  party <- wholePartyRsvp guest
  (formWidget, formEnctype) <- generateFormPost $ rsvpMForm party
  defaultLayout $(widgetFile "rsvp")

postRsvpR :: Handler Html
postRsvpR = do
  guest <- getGuestId
  party <- wholePartyRsvp guest
  handleRsvpPost party   
  defaultLayout $(widgetFile "invite")

handleRsvpPost :: PartyRsvp -> Handler ()
handleRsvpPost rsvp = do
  ((res, widget), enctype) <- runFormPost $ rsvpMForm rsvp
  case res of
    FormSuccess gs -> do
      runDB $ do
        case gs of 
          Solo me -> updateRsvp me
          Couple one two -> do 
            updateRsvp one
            updateRsvp two
    _ -> do
      liftIO $ print "fail"
      pure ()
  
updateRsvp :: GuestRsvp -> DB ()
updateRsvp g@(GuestRsvp gid _ _ _) = do 
  deleteBy (UniqueRsvp gid)
  insert_ g

getGuestId :: Handler (GuestId)
getGuestId = do
  (egid, _) <- requireAuthPair
  pure egid

defaultRsvp gid = GuestRsvp gid True Nothing False
yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]


rsvpMForm :: PartyRsvp -> Html -> MForm Handler (FormResult PartyRsvp, Widget)
rsvpMForm (Solo g) e = do 
  (r, w) <- rsvpMForm' g e
  pure (Solo <$> r, w)
rsvpMForm (Couple o t) e = do 
  (r1, w1) <- rsvpMForm' o e
  (r2, w2) <- rsvpMForm' t e
  let w = [whamlet|
      ^{w1}
      ^{w2}
    |]
  pure (Couple <$> r1 <*> r2 , w)

rsvpMForm' :: NamedRsvp -> Html -> MForm Handler (FormResult NamedRsvp, Widget)
rsvpMForm' (GuestRsvp gid coming diet bus)  extra = do
  let comingW = mreq
              yesNo
              ("Can you come" {fsAttrs = [("class", "attending-control")]})
              (Just coming)
  let dietW = mopt textField ("Can you come" {fsAttrs = [("class", "dietcontrol"), ("placeholder", "I hate peas")]}) (Just diet)
  let busW = mreq
              yesNo
              ("Do you need a transfer between the reception and the venue" {fsAttrs = [("class", "bus-control")]})
              (Just bus)
  (comingRes, comingView) <- comingW

  (dietRes, dietView) <- dietW 
  (busRes, busView) <- busW
  let widget = [whamlet|
      #{extra}
      ^{fvInput comingView}
      ^{fvInput busView}
      ^{fvInput dietView}
    |]
  let guestRes = GuestRsvp gid <$> comingRes <*> dietRes <*> busRes 
  pure (guestRes, widget)

wholePartyRsvp :: GuestId -> Handler PartyRsvp
wholePartyRsvp gid = runDB $ do
  rsvp <- lookupRsvp gid
  mplusone <- plusone gid
  case mplusone of
    Just poid -> do
      prsvp <- lookupRsvp poid 
      pure (Couple rsvp prsvp)
    _ -> pure (Solo rsvp)

lookupRsvp :: GuestId -> DB (GuestRsvp)
lookupRsvp i = maybe (defaultRsvp i) (\(Entity _ rsvp) -> rsvp) <$> (getBy $ UniqueRsvp i)

plusone :: GuestId -> DB (Maybe GuestId)
plusone g = do
    mpid <- get g  
    case mpid of 
      Just (Guest _ _ _ partyId) -> do 
          eguests <- lookupParty partyId
          let others = filter (\k -> k /= g) $ (\(Entity k _) -> k) <$> eguests
          pure (listToMaybe others)
      _ -> pure Nothing

lookupParty :: Party -> DB [Entity Guest]
lookupParty partyId = selectList [GuestParty ==. partyId] [LimitTo 2]