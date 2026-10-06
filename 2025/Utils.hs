-- | Common Utilities
--
-- Provides puzzle input fetching, caching, and environment configuration for 2025 Haskell solutions.
module Utils (getInputData) where

import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readCreateProcessWithExitCode, shell)

-- | Retrieves the puzzle input for a given day, reading from local cache or fetching via HTTP.
getInputData :: Int -> IO String
getInputData day = do
  let path = "data" </> (twoDigits day ++ ".txt")
  exists <- doesFileExist path
  if exists
    then readFile path
    else downloadAndCache day path

-- | Downloads puzzle input from Advent of Code and caches it to disk.
downloadAndCache :: Int -> FilePath -> IO String
downloadAndCache day path = do
  session <- getSessionCookie
  let url = concat ["https://adventofcode.com/2025/day/", show day, "/input"]
  let cookie = "Cookie: session=" ++ session
  let cmd = concat ["curl -sS -H \"", cookie, "\" \"", url, "\""]

  (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
  case code of
    ExitSuccess -> do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path out
      pure out
    ExitFailure _ -> fail $ "curl failed: " ++ trim (out ++ err)

-- | Retrieves the Advent of Code session cookie from the environment or parent @.env@ file.
getSessionCookie :: IO String
getSessionCookie = do
  env <- lookupEnv "AOC_SESSION_COOKIE"
  case env of
    Just tok | not (null tok) -> pure tok
    _ -> do
      cwd <- getCurrentDirectory
      let parent = takeDirectory cwd </> ".env"
      fromParent <- findEnvInFile parent
      case fromParent of
        Just tok | not (null tok) -> pure tok
        _ -> fail "AOC_SESSION_COOKIE not found in environment variables or .env file."

-- | Formats an integer day number as a two-digit string.
twoDigits :: Int -> String
twoDigits n
  | n < 10 = '0' : show n
  | otherwise = show n

-- | Trims leading and trailing whitespace characters from a string.
trim :: String -> String
trim = f . f where f = reverse . dropWhile (`elem` ['\n', '\r', '\t', ' '])

-- | Parses a @.env@ file to extract the value of @AOC_SESSION_COOKIE@.
findEnvInFile :: FilePath -> IO (Maybe String)
findEnvInFile fp = do
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      content <- readFile fp
      let ls = lines content
          key = "AOC_SESSION_COOKIE"
          m = foldr (pick key) Nothing ls
      pure m
  where
    pick key line acc =
      case acc of
        Just _ -> acc
        Nothing ->
          let l = dropWhile (\c -> c == ' ' || c == '\t') line
           in if null l || head l == '#'
                then Nothing
                else case break (== '=') l of
                  (k, '=' : v) | trim k == key -> Just (stripQuotes (trim v))
                  _ -> Nothing

-- | Strips surrounding single or double quotes from a string value.
stripQuotes :: String -> String
stripQuotes s =
  case s of
    ('"' : rest) | not (null rest) && last rest == '"' -> init rest
    ('\'' : rest) | not (null rest) && last rest == '\'' -> init rest
    _ -> s