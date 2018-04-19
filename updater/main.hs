{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger (NoLoggingT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Guest
import Database.Persist
import Database.Persist.Postgresql
import Import hiding ((.=))
import Options.Applicative as O

data Command
  = Upload String
  | Download String

data Inputs = Inputs
  { inputCommand :: Command
  , databaseConnection :: ConnectionString
  }

newtype NamedRsvp =
  MkNamedRsvp (Text, Maybe GuestRsvp)

instance ToNamedRecord NamedRsvp where
  toNamedRecord (MkNamedRsvp (n, Nothing)) =
    namedRecord ["Name" .= n, "Coming" .= na, "Diet" .= na, "Bus" .= na]
    where
      na :: Text
      na = "No reply"

instance DefaultOrdered NamedRsvp where
  headerOrder _ = ["Name", "Coming", "Diet", "Bus"]

main :: IO ()
main = do
  options <- execParser opts
  let conn = databaseConnection options
  case inputCommand options of
    Download file -> downloadRsvp conn file
    Upload file -> updateGuestList conn file

opts :: O.ParserInfo Inputs
opts =
  info
    (inputs <**> helper)
    (fullDesc <> progDesc "Link data between spreadsheet and database" <>
     O.header "guest updater")

inputs :: O.Parser Inputs
inputs =
  let cmd =
        subparser
          (command
             "upload"
             (info
                (Upload <$> (strArgument (metavar "GUESTLISTFILE")))
                (progDesc "Update the guest list")) <>
           command
             "download"
             (info
                (Download <$> (strArgument (metavar "TARGETFILE")))
                (progDesc "Update the guest list")))
  in Inputs <$> cmd <*>
     option
       auto
       (long "connection" <> short 'c' <> metavar "CONNECTION" <> value connStr)

decodeGuests :: BL.ByteString -> Either String (Vector Guest)
decodeGuests b =
  map (\(usrnme, f, l, h) -> createGuest usrnme f l h) <$> decodeGuests' b

decodeGuests' ::
     BL.ByteString
  -> Either String (Vector (GuestUserName, FirstName, LastName, Party))
decodeGuests' b = decode HasHeader b

encodeRsvp :: [NamedRsvp] -> BL.ByteString
encodeRsvp rs = encodeDefaultOrderedByName rs

connStr =
  "host=localhost dbname=postgres user=postgres password=mysecretpassword port=4321"

type HandleDB m a = ReaderT SqlBackend m a

updateGuestList :: ConnectionString -> String -> IO ()
updateGuestList conn file = do
  csvcontents <- BL.readFile file
  let egs = decodeGuests csvcontents
  case egs of
    Left er -> putStrLn (pack er)
    Right gs -> updateGuests conn gs

downloadRsvp :: ConnectionString -> String -> IO ()
downloadRsvp conn file = do
  rsvps <- getRsvps conn
  let csv = encodeRsvp rsvps
  BL.writeFile file csv

runDbCommand ::
     ConnectionString -> HandleDB (NoLoggingT (ResourceT IO)) a -> IO a
runDbCommand conn cmd = do
  runStderrLoggingT $
    withPostgresqlPool conn 10 $ \pool ->
      liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          cmd

updateGuests :: ConnectionString -> Vector Guest -> IO ()
updateGuests conn gs = do
  _ <- runDbCommand conn $ traverse upsertGuest gs
  pure ()

upsertGuest :: MonadIO m => Guest -> HandleDB m ()
upsertGuest g@(Guest usrname _ _ _) = do
  mg <- getBy $ UniqueGuest usrname
  case mg of
    Nothing -> insert_ g
    Just (Entity gid _) -> replace gid g

getRsvps :: ConnectionString -> IO [NamedRsvp]
getRsvps conn = runDbCommand conn readRsvp

readRsvp :: MonadIO m => HandleDB m [NamedRsvp]
readRsvp = pure []
