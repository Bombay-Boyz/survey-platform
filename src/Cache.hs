module Cache
  ( Cache
  , newCache
  , getCached
  , setCached
  , invalidate
  , purgeExpired
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict          (Map)
import Data.Text                (Text)
import Data.Time                (NominalDiffTime, UTCTime, addUTCTime,
                                 getCurrentTime)

data CacheEntry a = CacheEntry
  { entryValue   :: a
  , entryExpires :: UTCTime
  }

newtype Cache a = Cache (TVar (Map Text (CacheEntry a)))

newCache :: IO (Cache a)
newCache = Cache <$> newTVarIO Map.empty

-- | O(log n). Returns Nothing if absent or expired; evicts stale entry atomically.
getCached :: Cache a -> Text -> IO (Maybe a)
getCached (Cache var) key = do
  now <- getCurrentTime
  atomically $ do
    m <- readTVar var
    case Map.lookup key m of
      Nothing -> pure Nothing
      Just e
        | entryExpires e > now -> pure (Just (entryValue e))
        | otherwise -> do
            modifyTVar' var (Map.delete key)
            pure Nothing

-- | O(log n). Insert or overwrite with a TTL in seconds.
setCached :: Cache a -> Text -> a -> NominalDiffTime -> IO ()
setCached (Cache var) key val ttl = do
  now <- getCurrentTime
  let e = CacheEntry val (addUTCTime ttl now)
  atomically $ modifyTVar' var (Map.insert key e)

-- | O(log n). Remove a key immediately.
invalidate :: Cache a -> Text -> IO ()
invalidate (Cache var) key =
  atomically $ modifyTVar' var (Map.delete key)

-- | O(n). Evict all expired entries. Call periodically to cap memory.
purgeExpired :: Cache a -> IO ()
purgeExpired (Cache var) = do
  now <- getCurrentTime
  atomically $ modifyTVar' var (Map.filter (\e -> entryExpires e > now))
