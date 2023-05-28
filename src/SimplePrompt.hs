module SimplePrompt (
  prompt,
  promptInitial,
  promptBuffered,
  promptNonEmpty,
  promptEnter,
  yesNo,
  yesNoDefault
  ) where

import Control.Monad (unless)
import Data.List.Extra (lower, trim)

import SimplePrompt.Internal

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input (using timedInput)
--
-- Ignores buffered input line (ie if received in under 5ms)
prompt :: (MonadIO m, MonadMask m) => String -> m String
prompt = runPrompt . timedInput . getPromptLine

-- | reads string with initial input (using timedInput)
promptInitial :: (MonadIO m, MonadMask m) => String -> String -> m String
promptInitial s = runPrompt . timedInput . getPromptInitial s

-- | reads string with buffering
promptBuffered :: (MonadIO m, MonadMask m) => String -> m String
promptBuffered = runPrompt . getPromptLine

-- | reads non-empty string (using readNonEmpty)
promptNonEmpty :: (MonadIO m, MonadMask m) => String -> m String
promptNonEmpty = runPrompt . nonEmptyInput . getPromptLine

-- | prompt for Enter key
promptEnter :: (MonadIO m, MonadMask m) => String -> m ()
promptEnter s =
  runPrompt loop
  where
    loop = do
      c <- timedInput $ getPromptChar (s ++ ": ")
      unless (c == '\n') loop

-- | Yes/No prompt
yesNo :: (MonadIO m, MonadMask m) => String -> m Bool
yesNo desc = do
  runPrompt loop
  where
    loop = do
      inp <- nonEmptyInput $ getPromptLine $ desc ++ "? [y/n]"
      case trim (lower inp) of
        "y" -> return True
        "yes" -> return True
        "n" -> return False
        "no" -> return False
        _ ->  loop

yesNoDefault :: (MonadIO m, MonadMask m) => Bool -> String -> m Bool
yesNoDefault yes desc =
  runPrompt loop
  where
    loop = do
      inp <- timedInput $ getPromptLine $ desc ++ "? " ++ if yes then "[Y/n]" else "[y/N]"
      case trim (lower inp) of
        "y" -> return True
        "yes" -> return True
        "n" -> return False
        "no" -> return False
        "" -> return yes
        _ ->  loop
