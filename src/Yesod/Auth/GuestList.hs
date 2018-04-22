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

import Control.Applicative ((<$>), (<*>))
import Import.NoFoundation
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
import Data.Text (toTitle)

loginR :: AuthRoute
loginR = PluginR "guestlist" ["login"]

class YesodAuth site => YesodGuestList site where
  isGuestOnList :: Text -> Text -> AuthHandler site (Either [(Text, Text)] Text)

authGuestList :: YesodGuestList site => AuthPlugin site
authGuestList = AuthPlugin "guestlist" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget toMaster = do
      request <- getRequest
      $(widgetFile "login")

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
    Left xs -> do
      setMessageI (suggestionError (firstname <> " " <> lastname) xs)
      redirect LoginR

suggestionError :: Text -> [(Text, Text)] -> Text
suggestionError n [] = loginErrorStart n <> loginErrorTechSupport
suggestionError n xs =
  loginErrorStart n <> "Perhaps you meant: " <>
  (intercalate ", " ((\(x, y) -> (toTitle x) <> " " <> (toTitle y)) <$> xs)) <> ". " <> loginErrorTechSupport

loginErrorStart n =  "Unable to find a guest named '" <> n <> "'. "
loginErrorTechSupport = "If you are having problems logging in, please contact tobs169@gmail.com."
