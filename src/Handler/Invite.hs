{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Invite where

import Data.Guest
import Import
import Yesod

type NamedRsvp = (Guest, GuestRsvp)

data PartyRsvp
  = Solo NamedRsvp
  | Couple NamedRsvp
           NamedRsvp
  deriving (Show)

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
  guest <- getGuest
  party <- wholePartyRsvp guest
  ((res, formWidget), formEnctype) <- runFormPost $ rsvpMForm party
  defaultLayout $(widgetFile "rsvp")

postRsvpR :: Handler Html
postRsvpR = do
  guest <- getGuest
  party <- wholePartyRsvp guest
  liftIO $ print party
  handleRsvpPost party

handleRsvpPost :: PartyRsvp -> Handler Html
handleRsvpPost rsvp = do
  ((res, widget), enctype) <- runFormPost $ rsvpMForm rsvp
  liftIO $ print res
  case res of
    FormSuccess gs -> do
      runDB $ do
        case gs of
          Solo me -> updateRsvp (snd me)
          Couple one two -> do
            updateRsvp (snd one)
            updateRsvp (snd two)
      defaultLayout $(widgetFile "invite")
    FormFailure pageErrors -> do
      defaultLayout $(widgetFile "errors")

updateRsvp :: GuestRsvp -> DB ()
updateRsvp g@(GuestRsvp gid _ _ _) = do
  deleteBy (UniqueRsvp gid)
  insert_ g

getGuest :: Handler (Entity Guest)
getGuest = requireAuth

defaultRsvp gid = GuestRsvp gid True Nothing False

yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]

rsvpMForm :: PartyRsvp -> Html -> MForm Handler (FormResult PartyRsvp, Widget)
rsvpMForm (Solo g) e = do
  (r, w) <- rsvpMForm' g e
  pure (Solo <$> r, w)
rsvpMForm (Couple o t) e = do
  (r1, w1) <- rsvpMForm' o e
  (r2, w2) <- rsvpMForm' t mempty
  let w =
        [whamlet|
      ^{w1}
      ^{w2}
    |]
  pure (Couple <$> r1 <*> r2, w)

rsvpMForm' :: NamedRsvp -> Html -> MForm Handler (FormResult NamedRsvp, Widget)
rsvpMForm' (guest, GuestRsvp gid coming diet bus) extra = do
  let comingW =
        mreq
          yesNo
          ("Can you come" {fsAttrs = [("class", "attending-control")]})
          (Just coming)
  let dietW =
        mopt
          textField
          ("Can you come"
           { fsAttrs =
               [("class", "dietcontrol"), ("placeholder", "I hate peas")]
           })
          (Just diet)
  let busW =
        mreq
          yesNo
          ("Do you need a transfer between the reception and the venue"
           {fsAttrs = [("class", "bus-control")]})
          (Just bus)
  (comingRes, comingView) <- comingW
  (dietRes, dietView) <- dietW
  (busRes, busView) <- busW
  let name = prettyName guest
  let widget =
        [whamlet|
      #{extra}
      <h4> #{name}
      <div>
        <label> 
          Are you able to attend?
        ^{fvInput comingView}
      <div> 
        <label>
          Will you need transport between the reception and the venue?
        ^{fvInput busView}
      <div>
        <label>
          Do you have any dietary requirements?
        ^{fvInput dietView}
    |]
  let guestRes =
        (,) guest <$> (GuestRsvp gid <$> comingRes <*> dietRes <*> busRes)
  pure (guestRes, widget)

wholePartyRsvp :: Entity Guest -> Handler PartyRsvp
wholePartyRsvp g =
  runDB $ do
    rsvp <- lookupRsvp g
    mplusone <- plusone g
    case mplusone of
      Just poid -> do
        prsvp <- lookupRsvp poid
        pure (Couple rsvp prsvp)
      _ -> pure (Solo rsvp)

lookupRsvp :: Entity Guest -> DB (NamedRsvp)
lookupRsvp (Entity i g) = do
  rsvp <-
    maybe (defaultRsvp i) (\(Entity _ rsvp) -> rsvp) <$> (getBy $ UniqueRsvp i)
  pure (g, rsvp)

plusone :: Entity Guest -> DB (Maybe (Entity Guest))
plusone (Entity g (Guest _ _ _ partyId)) = do
  eguests <- lookupParty partyId
  let others = filter (\(Entity k _) -> k /= g) $ eguests
  pure (listToMaybe others)

lookupParty :: Party -> DB [Entity Guest]
lookupParty partyId = selectList [GuestParty ==. partyId] [LimitTo 2]
