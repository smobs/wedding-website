{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Guest
import Database.Persist
import Database.Persist.Postgresql
import Import

main :: IO ()
main = do
  let egs = decodeGuests "TobySmyth ,Toby,Smyth,0\r\nJenniferEllis,Jennifer,Ellis,0"
  case egs of
    Left er -> putStrLn (pack er)
    Right gs -> updateGuests gs

decodeGuests :: BL.ByteString -> Either String (Vector Guest)
decodeGuests b =
  map (\(usrnme, f, l, h) -> createGuest usrnme f l h) <$> decodeGuests' b

decodeGuests' ::
     BL.ByteString
  -> Either String (Vector (GuestUserName, FirstName, LastName, Party))
decodeGuests' b = decode NoHeader b

connStr =
  "host=localhost dbname=postgres user=postgres password=mysecretpassword port=4321"

type HandleDB m a = ReaderT SqlBackend m a

updateGuests :: Vector Guest -> IO ()
updateGuests gs = do
  runStderrLoggingT $
    withPostgresqlPool connStr 10 $ \pool ->
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
