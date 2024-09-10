module Main where

import Control.DeepSeq (deepseq)
import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs hiding (args, name)
import System.Directory
import System.FilePath ((</>))

newtype Args = CustomName {name :: Maybe String} deriving (Show, Data, Typeable)

userArgs :: Args
userArgs = CustomName {name = Nothing &= help "Name for the Mark. If unset the current folders name will be taken."}

main :: IO ()
main = do
  args <- cmdArgs userArgs
  dir <- getCurrentDirectory
  let folder = reverse $ takeWhile (/= '/') $ reverse dir
  let n = fromMaybe folder $ name args
  savePath n dir

savePath :: String -> FilePath -> IO ()
savePath name path = do
  homeDir <- getHomeDirectory
  let cfgDir = homeDir </> ".config/mrk"
  createDirectoryIfMissing True cfgDir
  let locationsDir = cfgDir </> "locations"
  prev <- try $ readFile locationsDir :: IO (Either IOException String)
  content <- case prev of
    Left _ -> return ""
    Right content -> return content
  content `deepseq` return ()
  let filtered = filter (not . isPrefixOf name) $ lines content
  let updated = unlines $ (name ++ ":" ++ path) : filtered
  writeFile locationsDir updated
  putStrLn $ "Wrote location " ++ path ++ " as mark '" ++ name ++ "'!"
