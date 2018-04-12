{-# LANGUAGE NoImplicitPrelude #-}

module Data.Guest
  ( createGuest
  , FirstName
  , LastName
  , Household
  , GuestUserName
  , makeConsistent
  , prettyName
  ) where

import Data.Text (strip, toTitle)
import Import

type FirstName = Text

type LastName = Text

type GuestUserName = Text

type Household = Int

prettyName :: Guest -> Text
prettyName (Guest _ f l _) = (toTitle f) <> (toTitle l)

createGuest :: GuestUserName -> FirstName -> LastName -> Household -> Guest
createGuest u f l h =
  Guest (makeConsistent u) (makeConsistent f) (makeConsistent l) h

makeConsistent :: Text -> Text
makeConsistent = toLower . strip
