{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Guest
import Database.Persist
import Database.Persist.Postgresql
import Import
import Options.Applicative as O

data Command
  = Upload String
  | Download String

data Inputs = Inputs
  { inputCommand :: Command
  , databaseConnection :: ConnectionString
  }

main :: IO ()
main = do
  options <- execParser opts
  let conn = databaseConnection options
  case inputCommand options of
    Download fileName -> pure ()
    Upload fileName -> do
      csvcontents <- BL.readFile fileName
      let egs = decodeGuests csvcontents
      case egs of
        Left er -> putStrLn (pack er)
        Right gs -> updateGuests conn gs

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

connStr =
  "host=localhost dbname=postgres user=postgres password=mysecretpassword port=4321"

type HandleDB m a = ReaderT SqlBackend m a

updateGuests :: ConnectionString -> Vector Guest -> IO ()
updateGuests conn gs = do
  runStderrLoggingT $
    withPostgresqlPool conn 10 $ \pool ->
      liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          _ <- traverse upsertGuest gs
          pure ()

upsertGuest :: MonadIO m => Guest -> HandleDB m ()
upsertGuest g@(Guest usrname _ _ _) = do
  mg <- getBy $ UniqueGuest usrname
  case mg of
    Nothing -> insert_ g
    Just (Entity gid _) -> replace gid g
