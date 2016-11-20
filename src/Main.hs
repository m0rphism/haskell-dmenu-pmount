{-# LANGUAGE UnicodeSyntax, LambdaCase, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Exception
import Data.Maybe
import Data.List (isPrefixOf, sort)

import qualified DMenu

listDirectoryDef
  :: MonadIO m
  => FilePath
  -> m [FilePath]
listDirectoryDef p =
  liftIO $ fmap sort $
    listDirectory p `catch` (\(_ :: SomeException) -> pure [])

getDevs
  :: MonadIO m
  => m [String]
getDevs =
  liftIO $ listDirectoryDef "/dev"

runProc
  :: MonadIO m
  => String
  -> [String]
  -> String
  -> m (Either String String)
runProc prog args sIn =
  liftIO $ do
    (exitCode, sOut, sErr) ←
      readCreateProcessWithExitCode (proc prog args) sIn
    pure $ case exitCode of
      ExitSuccess   → Right sOut
      ExitFailure _ → Left sErr

runProcOr
  :: MonadIO m
  => String
  -> [String]
  -> String
  -> String
  -> m String
runProcOr prog args sIn sDef =
  either (const sDef) id <$> runProc prog args sIn

filterWithPrefixes
  :: [String]
  -> [String]
  -> [String]
filterWithPrefixes ps =
  filter $ \s → any (`isPrefixOf` s) ps

devPathPrefixes
  :: [String]
devPathPrefixes =
  [ "sd" ]

devPathPrefixesU
  :: [String]
devPathPrefixesU =
  map ("/dev/" ++) [ "sd" ]

-- | Drop an amount of number substrings separated by whitespace from a string.
skipNumbersAndWS
  :: Int
  -> String
  -> String
skipNumbersAndWS =
  go False
 where
  go _ 0 s                           = dropWhile (`elem` [' ', '\t']) $ s
  go _ _ ""                          = ""
  go b n (c:s) | c `elem` ['0'..'9'] = go True n s
               | otherwise           = go False (if b then n-1 else n) s

-- Display a byte size as 'String'.
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


getPartitionInfos
  :: MonadIO m
  => m [(FilePath, String)]
getPartitionInfos = do
  sOut ← runProcOr "cat" ["/proc/partitions"] "" ""
  forM (drop 2 $ lines sOut) $ \l → do
    let numBlocks :: Integer = read $ head $ words $ skipNumbersAndWS 2 l
    let devPath = head (words $ skipNumbersAndWS 3 l)
    pure (devPath, showBytes 1024 $ numBlocks * 1024)

fillWithSP
  :: [String]
  -> [String]
fillWithSP ss =
  map f ss
 where
  f s = s ++ replicate (maxLength - length s) ' '
  maxLength = maximum (map length ss)

fillWithSPR
  :: [String]
  -> [String]
fillWithSPR ss =
  map f ss
 where
  f s = replicate (maxLength - length s) ' ' ++ s
  maxLength = maximum (map length ss)

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

main
  :: IO ()
main =
  getArgs >>= \case
    [] → do
      devs ← filterWithPrefixes devPathPrefixes <$> getDevs
      devInfos ← getPartitionInfos
      let devInfos' = map (fromMaybe "" . flip lookup devInfos) devs
      let devs' = zip devs $ zipWith (\x y → x++"  "++y) (fillWithSP devs) (fillWithSPR devInfos')
      DMenu.selectWith (DMenu.prompt .= "mount") snd devs' >>= \case
        Right (dev,_) | not (null dev) → callProcess "pmount" [dev]
                      | otherwise → pure ()
        Left (i, err) → putStrLn $ "DMenu failed with exit code " ++ show i ++ ": " ++ err
    ["-u"] → do
      devs ← map (drop 5) . filterWithPrefixes devPathPrefixesU <$> getMountedDevs
      devInfos ← getPartitionInfos
      let devInfos' = map (fromMaybe "" . flip lookup devInfos) devs
      let devs' = zip devs $ zipWith (\x y → x++"  "++y) (fillWithSP devs) (fillWithSPR devInfos')
      DMenu.selectWith (DMenu.prompt .= "umount") snd devs' >>= \case
        Right (dev,_) | not (null dev) → callProcess "pumount" [dev]
                      | otherwise → pure ()
        Left (i, err) → putStrLn $ "DMenu failed with exit code " ++ show i ++ ": " ++ err
      -- DMenu.runAsk (DMenu.prompt .= "umount") devs >>= \case
      --   Right (dev:_) | not (null dev) → callProcess "pumount" [dev]
      --   _             → pure ()
    _ → do
      putStrLn usage
      exitFailure

usage
  :: String
usage =
  unlines
    [ "Usage: dmenu-pmount [-u]"
    , ""
    , "Options:"
    , "  -u: unmount devices with pumount."
    ]
