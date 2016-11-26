{-# LANGUAGE UnicodeSyntax, LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Text.Read (readMaybe)
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Exception
import Data.Maybe
import Data.List (isPrefixOf, sort)

import qualified DMenu

-- | List the files in a directory or the empty list in case of an exception.
listDirectoryDef
  :: MonadIO m
  => FilePath
  -> m [FilePath]
listDirectoryDef p =
  liftIO $ fmap sort $
    listDirectory p `catch` (\(_ :: SomeException) -> pure [])

-- | Return all files in the /dev/ directory.
getDevs
  :: MonadIO m
  => m [FilePath]
getDevs =
  liftIO $ listDirectoryDef "/dev"

-- | Run a process.
runProc
  :: MonadIO m
  => String   -- ^ Program name
  -> [String] -- ^ Program arguments
  -> String   -- ^ Input piped into @stdin@
  -> m (Either String String) -- ^ Either the @stderr@ output, if the process failed, or the @stdout@ output.
runProc prog args sIn =
  liftIO $ do
    (exitCode, sOut, sErr) ←
      readCreateProcessWithExitCode (proc prog args) sIn
    pure $ case exitCode of
      ExitSuccess   → Right sOut
      ExitFailure _ → Left sErr

-- | Run a process.
runProcOr
  :: MonadIO m
  => String   -- ^ Program name
  -> [String] -- ^ Program arguments
  -> String   -- ^ Input piped into @stdin@
  -> String   -- ^ Default value, returned if process exits with failure
  -> m String -- ^ Either the default value, or the @stdout@ output of the process
runProcOr prog args sIn sDef =
  either (const sDef) id <$> runProc prog args sIn

-- | Keep 'String's which have certain prefixes.
filterWithPrefixes
  :: [String] -- ^ List of prefixes
  -> [String] -- ^ List of 'String's to filter
  -> [String] -- ^ 'String's which have one of the prefixes as prefix.
filterWithPrefixes = \case
  [] -> id
  ps -> filter $ \s → any (`isPrefixOf` s) ps

-- | Drop an amount of number substrings separated by whitespace from a string.
skipNumbersAndWS
  :: Int -- ^ How many numbers to skip
  -> String
  -> String
skipNumbersAndWS =
  go False
 where
  go _ 0 s                           = dropWhile (`elem` [' ', '\t']) $ s
  go _ _ ""                          = ""
  go b n (c:s) | c `elem` ['0'..'9'] = go True n s
               | otherwise           = go False (if b then n-1 else n) s

-- | Display a byte size as 'String'.
showBytes
  :: Integer -- ^ Number of bytes for @1 KB@. Usually @1024@ or @1000@.
  -> Integer -- ^ Number of bytes to display as 'String'.
  -> String
showBytes k i
  | i < k         = show i ++ " B"
  | i < k*k       = show (i `div` k) ++ " KB"
  | i < k*k*k     = show (i `div` (k*k)) ++ " MB"
  | i < k*k*k*k   = show (i `div` (k*k*k)) ++ " GB"
  | i < k*k*k*k*k = show (i `div` (k*k*k*k)) ++ " TB"
  | otherwise     = show (i `div` (k*k*k*k*k)) ++ " PB"

-- | Run @cat /proc/partitions@ and collect info about the capacities of
-- storage devices.
getPartitionInfos
  :: MonadIO m
  => m [(FilePath, Integer)] -- ^ Map from device name to @String@ representation of it's capacity.
getPartitionInfos = do
  sOut ← runProcOr "cat" ["/proc/partitions"] "" ""
  forM (drop 2 $ lines sOut) $ \l → do
    let numBlocks :: Integer = read $ head $ words $ skipNumbersAndWS 2 l
    let devPath = head (words $ skipNumbersAndWS 3 l)
    pure (devPath, numBlocks * 1024)

-- | Make all @String@s in a list have the same size, by filling missing
-- characters with spaces from the right.
fillWithSpR
  :: [String]
  -> [String]
fillWithSpR ss =
  map f ss
 where
  f s = s ++ replicate (maxLength - length s) ' '
  maxLength = maximum (map length ss)

-- | Make all @String@s in a list have the same size, by filling missing
-- characters with spaces from the left.
fillWithSpL
  :: [String]
  -> [String]
fillWithSpL ss =
  map f ss
 where
  f s = replicate (maxLength - length s) ' ' ++ s
  maxLength = maximum (map length ss)

-- | Spawn a @pmount@ process and retrieve a list of currently mounted devices.
getMountedDevs
  :: MonadIO m
  => m [FilePath]
getMountedDevs =
  liftIO $ do
    (exitCode, sOut, _sErr) <- readCreateProcessWithExitCode
      (proc "pmount" []) ""
    pure $ case exitCode of
      ExitSuccess   → map ((!! 0) . words) $ drop 1 $ init $ lines sOut
      ExitFailure _ → []

-- | Parse the command line arguments
readArgs
  :: [String] -- ^ Arguments from 'getArgs'
  -> IO (Bool, [String], Integer) -- ^ (1) Whether the unmount flag was set, (2) a list of device prefixes to filter for, (3) how much byte are 1 KB.
readArgs args =
  execStateT (go $ words $ unwords args) (False, [], 1024)
 where
  go [] =
    pure ()
  go (a:as)
    -- All arguments after "--" are passed to dmenu later.
    | a == "--" =
      pure ()
    | a `elem` ["-f", "--filter"] = do
      let (prefixes, as') = break ("-" `isPrefixOf`) as
      _2 %= (++prefixes)
      go as'
    | a `elem` ["-u", "--unmount"] = do
      _1 .= True
      go as
    | a `elem` ["-k", "--kilo"]
    , a':as' <- as
    , Just k <- readMaybe a'
    , k > 0 = do
      _3 .= k
      go as'
    | a == "" =
      go as
  go _ =
    liftIO $ do
      putStrLn usage
      exitFailure

main
  :: IO ()
main = do
  (unmount, prefixes, k) <- readArgs =<< getArgs
  let (prog, devsM) | unmount = ("pumount", map (drop 5) . filterWithPrefixes (map ("/dev/"++) prefixes) <$> getMountedDevs)
                    | otherwise = ("pmount", filterWithPrefixes prefixes <$> getDevs)
  devs ← devsM
  devInfos ← map (_2 %~ showBytes k) <$> getPartitionInfos
  let devInfos' = map (fromMaybe "" . flip lookup devInfos) devs
  let devs' = zip devs $ zipWith (\x y → x++"  "++y) (fillWithSpR devs) (fillWithSpL devInfos')
  let dmenuOpts = do
        DMenu.prompt .= drop 1 prog
        DMenu.forwardExtraArgs
  DMenu.selectWith dmenuOpts snd devs' >>= \case
    Right (dev,_) | not (null dev) → callProcess prog [dev]
    _                              → pure ()

usage
  :: String
usage =
  unlines
    [ "USAGE"
    , "  dmenu-pmount [OPTIONS] [-- DMENUOPTIONS]"
    , ""
    , "  Mount or unmount a linux device with dmenu and pmount as user."
    , ""
    , "  All arguments, after the first `--` argument, are directly passed to dmenu."
    , ""
    , "OPTIONS"
    , "  -u, --unmount"
    , "    Unmount devices displayed by `pmount` with `pumount`."
    , "  -f, --filter <DevPrefixList>"
    , "    Only display devices whose filenames have a certain prefix."
    , "    Example: `dmenu-pmount -f sd cdrom` only displays devices beginning with"
    , "             `sd` or `cdrom`, e.g. `/dev/sda2`."
    , "  -k, --kilo <Natural>"
    , "    How much byte are represented by 1 KB? Usually 1024 (default) or 1000."
    ]
