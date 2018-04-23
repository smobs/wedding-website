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
  = PartyRsvp NamedRsvp [NamedRsvp]
  deriving (Show)

getInfoR :: Handler Html
getInfoR = do
  defaultLayout $(widgetFile "info")

getTravelR :: Handler Html
getTravelR = do
  defaultLayout $(widgetFile "getting-there")

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
          PartyRsvp x xs -> do
            traverse updateRsvp (snd <$> x : xs)
            pure ()
      setMessageI
        ("Thanks for RSVPing.  You can update your RSVP up until 30th June." :: Text)
      redirect InfoR
    FormFailure pageErrors -> do
      defaultLayout $(widgetFile "errors")

updateRsvp :: GuestRsvp -> DB ()
updateRsvp g@(GuestRsvp gid _ _ _ _) = do
  deleteBy (UniqueRsvp gid)
  insert_ g

getGuest :: Handler (Entity Guest)
getGuest = requireAuth

defaultRsvp gid = GuestRsvp gid True Nothing False ""

yesNo = radioFieldList [("Yes" :: Text, True), ("No" :: Text, False)]

rsvpMForm :: PartyRsvp -> Html -> MForm Handler (FormResult PartyRsvp, Widget)
rsvpMForm (PartyRsvp x xs) e = do
  (remail, wemail) <- emailForm x e 
  (r1, w1) <- rsvpMForm' x mempty
  others <- traverse (\y -> rsvpMForm' y mempty) xs
  let rs = traverse (\r -> (fst r) <*> remail) others
  let ws = snd <$> others 
  let w =
        [whamlet|
      <section .rsvpsection>
        ^{w1}
      $forall w' <- ws
        <section .rsvpsection>
          ^{w'}
      <section .rsvpsection>
        ^{wemail}
    |]
  pure (PartyRsvp <$> (r1 <*> remail) <*> rs, w)

emailForm :: NamedRsvp -> Html -> MForm Handler (FormResult Text, Widget)
emailForm (_, GuestRsvp _ _ _ _ email) extra = do
  (emailRes, emailView) <- mreq
          emailField
          ("Unused"
           { fsAttrs =
               [("class", "emailcontrol")]
           })
          (if email == "" then Nothing else Just email)
  let widget = [whamlet|
      #{extra}
      <div> 
        <label>
          Please provide an email address so we can contact you with updates about the wedding:
        ^{fvInput emailView}
    |]
  pure (emailRes, widget)


rsvpMForm' :: NamedRsvp -> Html -> MForm Handler (FormResult (Text -> NamedRsvp), Widget)
rsvpMForm' (guest, GuestRsvp gid coming diet bus _) extra = do
  let comingW =
        mreq
          yesNo
          ("Unused" {fsAttrs = [("class", "attending-control")]})
          (Just coming)
  let dietW =
        mopt
          textField
          ("Unused"
           { fsAttrs =
               [("class", "dietcontrol")]
           })
          (Just diet)
  let busW =
        mreq
          yesNo
          ("Unused"
           {fsAttrs = [("class", "bus-control")]})
          (Just bus)
  (comingRes, comingView) <- comingW
  (dietRes, dietView) <- dietW
  (busRes, busView) <- busW
  let name = prettyName guest
  let widget =
        [whamlet|
      #{extra}
      <h4 .rsvp-name> #{name}
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
        ((,) guest <$>) <$> (GuestRsvp gid <$> comingRes <*> dietRes <*> busRes)
  pure (guestRes, widget)

wholePartyRsvp :: Entity Guest -> Handler PartyRsvp
wholePartyRsvp g =
  runDB $ do
    rsvp <- lookupRsvp g
    poids <- plusones g
    prsvp <- traverse lookupRsvp poids
    pure (PartyRsvp rsvp prsvp)

lookupRsvp :: Entity Guest -> DB (NamedRsvp)
lookupRsvp (Entity i g) = do
  rsvp <-
    maybe (defaultRsvp i) (\(Entity _ rsvp) -> rsvp) <$> (getBy $ UniqueRsvp i)
  pure (g, rsvp)

plusones :: Entity Guest -> DB ( [Entity Guest])
plusones (Entity g (Guest _ _ _ partyId)) = do
  eguests <- lookupParty partyId
  let others = filter (\(Entity k _) -> k /= g) $ eguests
  pure others

lookupParty :: Party -> DB [Entity Guest]
lookupParty partyId = selectList [GuestParty ==. partyId] []
