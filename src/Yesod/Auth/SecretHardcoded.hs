{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.Auth.SecretHardcoded
  ( authSecretHardcoded
  , secretLoginWidget
  , loginR
  , YesodAuthHardcoded(..) )
  where

import           Yesod.Auth          (Auth, AuthPlugin (..), AuthRoute,
                                      Creds (..), Route (..), YesodAuth,
                                      loginErrorMessageI, setCredsRedirect)
import qualified Yesod.Auth.Message  as Msg
import           Yesod.Core
import           Yesod.Form          (ireq, runInputPost, textField)

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import Yesod.Auth.Hardcoded (YesodAuthHardcoded(..))

loginR :: AuthRoute
loginR = PluginR "secrethardcoded" ["login"]

authSecretHardcoded :: YesodAuthHardcoded m => AuthPlugin m
authSecretHardcoded =
  AuthPlugin "secrethardcoded" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget toMaster = 
        [whamlet|
            $newline never
            <div>
        |]


secretLoginWidget toMaster = do
      request <- getRequest
      [whamlet|
        $newline never
        <form method="post" action="@{toMaster loginR}">
          $maybe t <- reqToken request
            <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
          <table>
            <tr>
              <th>_{Msg.UserName}
              <td>
                 <input type="text" name="username" required>
            <tr>
              <th>_{Msg.Password}
              <td>
                 <input type="password" name="password" required>
            <tr>
              <td colspan="2">
                 <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
        |]


postLoginR :: (YesodAuthHardcoded master)
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR =
  do (username, password) <- lift (runInputPost
       ((,) Control.Applicative.<$> ireq textField "username"
            Control.Applicative.<*> ireq textField "password"))
     isValid <- lift (validatePassword username password)
     if isValid
        then lift (setCredsRedirect (Creds "secrethardcoded" username []))
        else do isExists <- lift (doesUserNameExist username)
                loginErrorMessageI LoginR
                                   (if isExists
                                       then Msg.InvalidUsernamePass
                                       else Msg.IdentifierNotFound username)