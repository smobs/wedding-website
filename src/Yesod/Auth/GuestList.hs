{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.GuestList
  ( loginR
  , authGuestList
  , YesodGuestList(..)
  ) where

import Yesod.Auth
  ( AuthHandler
  , AuthPlugin(..)
  , AuthRoute
  , Creds(..)
  , Route(..)
  , YesodAuth
  , loginErrorMessageI
  , setCredsRedirect
  )
import qualified Yesod.Auth.Message as Msg
import Yesod.Core
import Yesod.Form (ireq, runInputPost, textField)

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

loginR :: AuthRoute
loginR = PluginR "guestlist" ["login"]

class YesodAuth site =>
      YesodGuestList site
  where
  isGuestOnList :: Text -> Text -> AuthHandler site (Either [(Text, Text)] Text)

authGuestList :: YesodGuestList site => AuthPlugin site
authGuestList = AuthPlugin "guestlist" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget toMaster = do
      request <- getRequest
      [whamlet|
              $newline never
              <form method="post" action="@{toMaster loginR}">
                $maybe t <- reqToken request
                  <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
                <table>
                  <tr>
                    <th>First name
                    <td>
                       <input type="text" name="firstname" required>
                  <tr>
                    <th>Last name
                    <td>
                       <input type="text" name="lastname" required>
                  <tr>
                    <td colspan="2">
                       <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
              |]

postLoginR :: YesodGuestList site => AuthHandler site TypedContent
postLoginR = do
  (firstname, lastname) <-
    lift $
    runInputPost
      ((,) Control.Applicative.<$> ireq textField "firstname" Control.Applicative.<*>
       ireq textField "lastname")
  nameMatch <- isGuestOnList firstname lastname
  let usrname = (mappend firstname lastname)
  case nameMatch of
    Right guestId -> lift $ setCredsRedirect (Creds "guestlist" guestId [])
    Left _ -> loginErrorMessageI LoginR (Msg.IdentifierNotFound usrname)
