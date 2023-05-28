module SimplePrompt (
  prompt,
  prompt_,
  promptClear,
  yesNo
  ) where

import Control.Monad (void)
import Data.List.Extra (lower, trim)

import SimplePrompt.Internal

-- | reads string
prompt :: String -> IO String
prompt = runPrompt . getPromptOrError

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input
--
-- Ignores buffered input lines (ie received in under 5ms)
promptClear :: String -> IO String
promptClear =
  runPrompt . readClear

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
