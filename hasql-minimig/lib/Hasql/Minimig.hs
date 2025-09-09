{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- 1. @import "Hasql.Minimig" qualified as Hmm@
--
--
-- 2. List all the 'Migration's in ascending chronological order.
--    Each 'Migration' is a 'Ht.Transaction'al action on the database, identified
--    by a unique 'MigrationId'. Construct with 'Migration'.
--
--     @
--     __migrations__ :: ["Hmm".'Migration']
--     __migrations__ =
--        [ "Hmm".'Migration' /\"create users table\"/ createUsersTable
--        , "Hmm".'Migration' /\"add email column to users\"/ addUserEmailColumn
--        , "Hmm".'Migration' /\"create articles table\"/ createArticlesTable
--        , / ... more migrations ... /
--        ]
--     @
--
-- 3. Run any 'Migration's that haven't been run yet, if necessary, by performing
--    'migrate' once as soon as you obtain your 'Hasql.Connection.Connection',
--    or 'Hasql.Pool' perhaps.  'migrate' will enforce that the 'MigrationId's,
--    be unique, and will make sure that any migration history in the
--    migrations table is compatible with the specified 'MigrationId's.
--
-- 4. __Don't change your 'MigrationId's over time__. If you do, then the
--    history in the migrations table become unrecognizable by 'migrate'.
--    Also, avoid having the 'Transaction'al code in each 'Migration' use your
--    domain types and functions, as doing so may force you to alter past
--    migrations code if your domain types and functions change. Ideally, you
--    should write each 'Migration' in such a way that you never /have/ to
--    modify them in the future.
module Hasql.Minimig {--}
   ( -- * Runnning
    migrate

    -- * Migrations
   , Migration (..)
   , MigrationId (..)
   , fromDir

    -- * Miscellaneous
   , ErrMigrations (..)
   , history
   , ran
   ) -- }
where

import Control.Exception qualified as Ex
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString qualified as B
import Data.Functor.Contravariant
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Data.Time qualified as Time
import GHC.Show (appPrec)
import Hasql.Decoders qualified as Hd
import Hasql.Encoders qualified as He
import Hasql.Session qualified as Hs
import Hasql.Statement qualified as Hs
import Hasql.Transaction qualified as Ht
import Hasql.Transaction.Sessions qualified as Ht
import Numeric.Natural
import System.Directory qualified as D
import System.FilePath qualified as F
import Text.Parsec qualified as P

-- | A single 'Migration' consisting of a 'Ht.Transaction'al action uniquely
-- identified by a 'MigrationId'.
--
-- * Construct with 'migration'.
--
-- * Run through 'migrate'.
data Migration = Migration
   { id :: MigrationId
   , tx :: Ht.Transaction ()
   }

instance Show Migration where
   showsPrec n m =
      showParen (n > appPrec) $
         showString "Migration{id="
            . shows m.id
            . showString ", tx=..}"

-- | 'Just' if at least one @a@ is duplicate.
findDuplicate :: forall a. (Ord a) => [a] -> Maybe a
findDuplicate = go mempty
  where
   go :: Set a -> [a] -> Maybe a
   go !seen = \case
      a : rest
         | Set.member a seen -> Just a
         | otherwise -> go (Set.insert a seen) rest
      [] -> Nothing

-- | Unique identifier for a 'Migration' within a migrations table.
--
-- * You are supposed to type these statically, so construct a 'MigrationId'
-- by typing down the literal string.
newtype MigrationId = MigrationId {text :: T.Text}
   deriving newtype (Eq, Ord, IsString, Show)

--------------------------------------------------------------------------------

createMigrationsTable
   :: T.Text
   -- ^ Escaped name of the migrations table. Possibly schema-qualified.
   -> Ht.Transaction ()
createMigrationsTable tbl =
   -- We are storing the timestamp in case we need it in the future.
   -- We aren't really using it now.
   Ht.sql $
      "CREATE TABLE IF NOT EXISTS "
         <> fromString (T.unpack tbl)
         <> " (ord int4 PRIMARY KEY NOT NULL CHECK (ord >= 0)"
         <> ", time TIMESTAMPTZ NOT NULL"
         <> ", id TEXT UNIQUE NOT NULL)"

-- | Get the list already ran 'MigrationId's, in ascending chronological order.
ran
   :: T.Text
   -- ^ Escaped name of the migrations table. Possibly schema-qualified.
   -- Examples:
   --
   -- * @\"migrations\"@
   --
   -- * @\"public.migrations\"@
   --
   -- * @"\\"public\\".\\"My MiGrAtIoNs\\"\"@
   -> Hs.Statement () [MigrationId]
ran tbl =
   Hs.Statement
      ("SELECT id FROM " <> fromString (T.unpack tbl) <> " ORDER BY ord ASC")
      mempty
      (Hd.rowList (Hd.column (Hd.nonNullable (MigrationId <$> Hd.text))))
      False

pushMigration
   :: T.Text
   -- ^ Escaped name of the migrations table. Possibly schema-qualified.
   -> Hs.Statement MigrationId Time.UTCTime
pushMigration tbl =
   Hs.Statement
      ( "INSERT INTO "
         <> fromString (T.unpack tbl)
         <> " (ord, time, id)"
         <> " SELECT t.ord, now(), $1"
         <> " FROM (SELECT coalesce(max(ord) + 1, 0) AS ord FROM "
         <> fromString (T.unpack tbl)
         <> ") AS t"
         <> " RETURNING time"
      )
      (He.param (He.nonNullable ((.text) >$< He.text)))
      (Hd.singleRow $ Hd.column (Hd.nonNullable Hd.timestamptz))
      True

--------------------------------------------------------------------------------

data ErrMigrations
   = -- | The 'MigrationId' appears twice in the list of desired 'Migration's.
     ErrMigrations_DuplicateId MigrationId
   | -- | The history of ran migrations here as payload is incompatible with
     -- the list of desired 'Migration's.
     ErrMigrations_History [MigrationId]
   | -- | Different migrations share the same order. Thrown by 'fromDir'.
     ErrMigrations_DuplicateOrder Natural
   | -- | Malformed migration file name. Thrown by 'fromDir'.
     ErrMigrations_BadFileName FilePath
   deriving stock (Eq, Show)
   deriving anyclass (Ex.Exception)

-- | Run all the migrations in 'Migration's that haven't been run yet.
--
-- If the migrations table doesn't exist, it will be created.
--
-- Returns the 'MigrationId's that were in the migrations table before, and
-- the ones that were just added, all in ascending chronological order.
-- @(old, new)@
migrate
   :: T.Text
   -- ^ Escaped name of the migrations table. Possibly schema-qualified.
   -- Examples:
   --
   -- * @\"migrations\"@
   --
   -- * @\"public.migrations\"@
   --
   -- * @"\\"public\\".\\"My MiGrAtIoNs\\"\"@
   -> [Migration]
   -- ^ 'Migration's representing the desired state of the database, in
   -- ascending chronological order.
   -> Hs.Session (Either ErrMigrations ([MigrationId], [MigrationId]))
migrate tbl want = Ht.transaction Ht.Serializable Ht.Write $ migrate' tbl want

-- | Like 'migrate', but a 'Ht.Transaction'.
migrate'
   :: T.Text
   -> [Migration]
   -> Ht.Transaction (Either ErrMigrations ([MigrationId], [MigrationId]))
migrate' tbl want = do
   createMigrationsTable tbl
   history tbl (fmap (.id) want) >>= \case
      Left e -> pure $ Left e
      Right (ranIds, pendIds) -> do
         forM_ (List.drop (length ranIds) want) \m -> do
            m.tx
            Ht.statement m.id $ pushMigration tbl
         pure $ Right (ranIds, pendIds)

-- | Get the list of 'MigrationId's that have already been run, and the ones
-- that would need to be executed to take the current database state to the
-- desired state. All in ascending chronological order.
--
-- @(ran, pending)@
history
   :: T.Text
   -- ^ Escaped name of the migrations table. Possibly schema-qualified.
   -- Examples:
   --
   -- * @\"migrations\"@
   --
   -- * @\"public.migrations\"@
   --
   -- * @"\\"public\\".\\"My MiGrAtIoNs\\"\"@
   -> [MigrationId]
   -- ^ 'Migration's representing the desired state of the database, in
   -- ascending chronological order.
   -> Ht.Transaction (Either ErrMigrations ([MigrationId], [MigrationId]))
history tbl wantIds = case findDuplicate wantIds of
   Just mId -> pure $ Left $ ErrMigrations_DuplicateId mId
   Nothing -> do
      ranIds <- Ht.statement () (ran tbl)
      pure $ case List.stripPrefix ranIds wantIds of
         Just pendIds -> Right (ranIds, pendIds)
         Nothing -> Left $ ErrMigrations_History ranIds

--------------------------------------------------------------------------------

-- | Given the path to a directory containing raw SQL migration files, obtain
-- a list of 'Migration's in an order suitable to use with 'mitgrate'.
--
-- The directory is expected to contain only files with the following name
-- format:
--
--     @<ORDER> '_' <ID> '.sql'@
--
-- Where @<ORDER>@ is a natural number with optional leading zeros, and @<ID>@
-- is any string that will serve as 'MigrationId'. For example, the following
-- are valid file names:
--
-- * @0_add_user_table.sql@
--
-- * @14_Silly name, but valid.sql@
--
-- * @007_.sql@
--
-- * @9__.sql.sql@
--
-- The migration files are sorted /numerically/ by its @<ORDER>@ value, with the
-- smallest @<ORDER>@ being the first migration.
--
-- If there are files other than the @.sql@ files in the directory, this
-- function will fail.
--
-- The contents of the @.sql@ file are put literally in a 'Ht.Transaction' in
-- the @tx@ field of the corresponding `Migration`.
--
-- Note: It's OK to change value of @<ORDER>@ over time, as long as the
-- relative ordering of migrations to each other remains the same. For example,
-- maybe you decide that instead of using sequential numbers for ordering, you
-- would like to use calendar dates in the @YYYYMMDD@ format. You could rename
-- your file from names like `021_delete_coupons.sql` to
-- `20252802_delete_coupons.sql`, and it will be alright. Just don't change the
-- @delete_coupons@ part because that's the 'MigrationId'.
fromDir :: (MonadIO m) => FilePath -> m [Migration]
fromDir fp = liftIO do
   xs <- fmap (fp F.</>) <$> D.listDirectory fp
   ms <- traverse fromFile xs
   fmap (Map.elems . fst) do
      foldM
         ( \(mo0, si0) (o, m) -> do
            (,)
               <$> Map.alterF
                  ( \case
                     Nothing -> pure $ Just m
                     Just _ -> Ex.throwIO $ ErrMigrations_DuplicateOrder o
                  )
                  o
                  mo0
               <*> Set.alterF
                  ( \case
                     False -> pure True
                     True -> Ex.throwIO $ ErrMigrations_DuplicateId m.id
                  )
                  m.id
                  si0
         )
         (Map.empty, Set.empty)
         ms

fromFile :: (MonadIO m) => FilePath -> m (Natural, Migration)
fromFile fp = liftIO do
   let fn = F.takeFileName fp
   case parseFileName fn of
      Left _ -> Ex.throwIO $ ErrMigrations_BadFileName fn
      Right (mOrd, mId) -> do
         sql <- B.readFile fp
         pure (mOrd, Migration mId (Ht.sql sql))

parseFileName :: String -> Either String (Natural, MigrationId)
parseFileName = \x -> first show $ P.parse p x x
  where
   p :: P.Parsec String () (Natural, MigrationId)
   p = do
      mOrd <- pNatural
      _ <- P.char '_'
      mId <- P.manyTill P.anyChar $ P.try (P.string ".sql" >> P.eof)
      pure (mOrd, fromString mId)
   pNatural :: P.Parsec String () Natural
   pNatural =
      foldl' (\ !b a -> b * 10 + (read [a])) 0 <$> P.many1 P.digit
