module SimplePrompt (
  prompt,
  prompt_,
  yesno
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.List.Extra (lower, trim)
import Data.Time.Clock

import System.Console.Haskeline

-- FIXME promptNonEmpty
prompt :: String -> IO String
prompt s = do
  runInputT defaultSettings loop
    where
      loop :: InputT IO String
      loop = do
        start <- liftIO getCurrentTime
        minput <- getInputLine $ s ++ ": "
        end <- liftIO getCurrentTime
        let diff = diffUTCTime end start
        if diff < 0.005
          then do
          outputStrLn $
            "ignoring buffered input: " ++ show diff ++ " too quick"
          loop
          else
          case minput of
            Nothing -> return ""
            Just input -> return input

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

yesno :: Maybe Bool -> String -> IO Bool
yesno mdefault desc = do
  inp <- prompt $ desc ++ "? " ++ maybe "[y/n]" (bool "[y/N]" "[Y/n]") mdefault
  case trim (lower inp) of
    "y" -> return True
    "yes" -> return True
    "n" -> return False
    "no" -> return False
    "" -> maybe (yesno Nothing desc) return mdefault
    _ ->  yesno mdefault desc
