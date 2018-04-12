{-# LANGUAGE NoImplicitPrelude #-}

module Data.Guest
  ( createGuest
  , FirstName
  , LastName
  , Party
  , GuestUserName
  , makeConsistent
  , prettyName
  ) where

import Data.Text (strip, toTitle)
import Import.NoFoundation

type FirstName = Text

type LastName = Text

type GuestUserName = Text

type Party = Int

prettyName :: Guest -> Text
prettyName (Guest _ f l _) = (toTitle f) <> (toTitle l)

createGuest :: GuestUserName -> FirstName -> LastName -> Party -> Guest
createGuest u f l h =
  Guest (makeConsistent u) (makeConsistent f) (makeConsistent l) h

makeConsistent :: Text -> Text
makeConsistent = toLower . strip
