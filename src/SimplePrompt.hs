module SimplePrompt (
  prompt,
  prompt_,
  promptClear,
  yesNo
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List.Extra (lower, trim)
import Data.Time.Clock

import System.Console.Haskeline

-- | reads non-empty string
prompt :: String -> IO String
prompt s =
  runInputT defaultSettings loop
    where
      loop :: InputT IO String
      loop =
        getInputLine (s ++ ": ") >>=
        maybe loop return

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input
--
-- Ignores input received in under 5ms
promptClear :: String -> IO String
promptClear s =
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

-- | prompt which ignores the input
prompt_ :: String -> IO ()
prompt_ = void <$> promptClear

-- | Yes/No prompt
yesNo :: String -> IO Bool
yesNo desc = do
  inp <- prompt $ desc ++ "? [y/n]"
  case trim (lower inp) of
    "y" -> return True
    "yes" -> return True
    "n" -> return False
    "no" -> return False
    _ ->  yesNo desc
