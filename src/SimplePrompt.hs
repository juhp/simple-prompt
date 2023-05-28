{-# LANGUAGE CPP #-}

module SimplePrompt (
  prompt,
  promptInitial,
  promptBuffered,
  promptNonEmpty,
  promptEnter,
  promptPassword,
  yesNo,
  yesNoDefault
  ) where

import Control.Monad (unless)
import Data.List.Extra (lower, trim)

import SimplePrompt.Internal

#include "monadconstraint.h"

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input (using timedInput)
--
-- Ignores buffered input line (ie if received in under 5ms)
prompt :: MONADCONSTRAINT => String -> m String
prompt = runPrompt . timedInput . getPromptLine

-- FIXME non-empty?
-- | reads string with initial input (using timedInput)
promptInitial :: MONADCONSTRAINT => String -> String -> m String
promptInitial s = runPrompt . timedInput . getPromptInitial s

-- | reads string with buffering
promptBuffered :: MONADCONSTRAINT => String -> m String
promptBuffered = runPrompt . getPromptLine

-- | reads non-empty string (using readNonEmpty)
promptNonEmpty :: MONADCONSTRAINT => String -> m String
promptNonEmpty = runPrompt . nonEmptyInput . getPromptLine

-- | prompt for Enter key
promptEnter :: MONADCONSTRAINT => String -> m ()
promptEnter s =
  runPrompt loop
  where
    loop = do
      c <- timedInput $ getPromptChar (s ++ ": ")
      unless (c == '\n') loop

promptPassword :: MONADCONSTRAINT => String -> m String
promptPassword = runPrompt . nonEmptyInput . getPromptPassword

-- | Yes/No prompt
yesNo :: MONADCONSTRAINT => String -> m Bool
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

yesNoDefault :: MONADCONSTRAINT => Bool -> String -> m Bool
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
