module Main (main) where

import Control.Exception qualified as Ex
import Control.Monad
import Data.Int
import Data.String (fromString)
import Hasql.Connection qualified as Hc
import Hasql.Decoders qualified as Hd
import Hasql.Minimig qualified as Hm
import Hasql.Session qualified as Hs
import Hasql.Statement qualified as Hs
import Hasql.Transaction qualified as Ht
import Paths_hasql_minimig qualified
import System.Environment (getEnv)
import System.FilePath qualified as F

--------------------------------------------------------------------------------

connect :: String -> IO Hc.Connection
connect s = Hc.acquire (fromString s) >>= either (fail . show) pure

main :: IO ()
main = do
   connString <- getEnv "HASQL_MINIMIG_TEST_CONNSTRING"
   putStrLn $ "HASQL_MINIMIG_TEST_CONNSTRING: " <> connString
   Ex.bracket (connect connString) Hc.release \conn -> do
      let runSess :: Hs.Session a -> IO a
          runSess = \s -> Hs.run s conn >>= either Ex.throwIO pure
          runMigs :: [Hm.Migration] -> IO ([Hm.MigrationId], [Hm.MigrationId])
          runMigs = \ms ->
            runSess (Hm.migrate "migs" ms) >>= either Ex.throwIO pure

      ([], []) <- runMigs []
      ([], []) <- runMigs []

      Left (Hm.ErrMigrations_DuplicateId dup0) <- Ex.try $ runMigs [migA, migA]
      when (dup0 /= migA') $ fail "dup0"

      ([], new1) <- runMigs migsAB
      when (new1 /= migsAB') $ fail "ran1"

      (ran2, []) <- runMigs migsAB
      when (ran2 /= migsAB') $ fail "ran2"

      Left (Hm.ErrMigrations_History ran3) <- Ex.try $ runMigs migsCD
      when (ran3 /= migsAB') $ fail "ran3"

      (ran4, []) <- runMigs migsAB
      when (ran4 /= migsAB') $ fail "ran4"

      [] <- runSess $ Hs.statement () st0

      (ran5, new5) <- runMigs (migsAB <> migsCD)
      when (ran5 /= migsAB') $ fail "ran5"
      when (new5 /= migsCD') $ fail "new5"

      Left (Hm.ErrMigrations_History ran6) <- Ex.try $ runMigs []
      when (ran6 /= migsAB' <> migsCD') $ fail "ran6"

      Left (Hm.ErrMigrations_History ran7) <- Ex.try $ runMigs migsAB
      when (ran7 /= migsAB' <> migsCD') $ fail "ran7"

      Left (Hm.ErrMigrations_History ran8) <- Ex.try $ runMigs migsCD
      when (ran8 /= migsAB' <> migsCD') $ fail "ran8"

      (ran9, []) <- runMigs (migsAB <> migsCD)
      when (ran9 /= migsAB' <> migsCD') $ fail "ran9"

      [7, 8] <- runSess $ Hs.statement () st0

      dd <- Paths_hasql_minimig.getDataDir

      Ex.try (Hm.fromDir (dd F.</> "test/migs/dirty")) >>= \case
         Left e
            | Just (Hm.ErrMigrations_BadFileName "README.txt") <- Ex.fromException e -> pure ()
            | otherwise -> fail $ "dirty: Unexpected exception: " <> show e
         Right xs -> fail $ "dirty: Unexpected success: " <> show xs

      Ex.try (Hm.fromDir (dd F.</> "test/migs/bad_ord")) >>= \case
         Left e
            | Just (Hm.ErrMigrations_DuplicateOrder 1) <- Ex.fromException e -> pure ()
            | otherwise -> fail $ "dirty: Unexpected exception: " <> show e
         Right xs -> fail $ "dirty: Unexpected success: " <> show xs

      Ex.try (Hm.fromDir (dd F.</> "test/migs/bad_id")) >>= \case
         Left e
            | Just (Hm.ErrMigrations_DuplicateId "hello") <- Ex.fromException e -> pure ()
            | otherwise -> fail $ "dirty: Unexpected exception: " <> show e
         Right xs -> fail $ "dirty: Unexpected success: " <> show xs

      let migsOK' = ["hello", "", "_", "_.sql", " wow, Fancy!"]
      migsOK <- Hm.fromDir (dd F.</> "test/migs/ok")
      when (migsOK' /= fmap (.id) migsOK) $ fail "migsOK: not OK!"
      (ranOK, newOK) <- runMigs (migsAB <> migsCD <> migsOK)
      when (ranOK /= migsAB' <> migsCD') $ fail "ranOK"
      when (newOK /= migsOK') $ fail "newOK"
      [7, 8, 20, 21, 22, 23, 24] <- runSess $ Hs.statement () st0

      pure ()
  where
   migA :: Hm.Migration
   migA = Hm.Migration "A" (pure ())

   migA' :: Hm.MigrationId
   migA' = migA.id

   migsAB' :: [Hm.MigrationId]
   migsAB' = fmap (.id) migsAB
   migsAB :: [Hm.Migration]
   migsAB =
      [ migA
      , Hm.Migration "B" do
         Ht.sql "CREATE TABLE t (x int4)"
      ]

   migsCD' :: [Hm.MigrationId]
   migsCD' = fmap (.id) migsCD

   migsCD :: [Hm.Migration]
   migsCD =
      [ Hm.Migration "C" (pure ())
      , Hm.Migration "D" do
         Ht.sql "INSERT INTO t (x) VALUES (7), (8)"
      ]

   st0 :: Hs.Statement () [Int32]
   st0 =
      Hs.Statement
         "SELECT x FROM t ORDER BY x"
         mempty
         (Hd.rowList (Hd.column (Hd.nonNullable Hd.int4)))
         False
