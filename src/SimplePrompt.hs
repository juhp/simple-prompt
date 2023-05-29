{-# LANGUAGE CPP #-}

module SimplePrompt (
  prompt,
  promptInitial,
  promptBuffered,
  promptNonEmpty,
  promptChar,
  promptEnter,
  promptPassword,
  yesNo,
  yesNoDefault
  ) where

import Control.Monad (void)
import Data.List.Extra (lower, trim)

import SimplePrompt.Internal

#include "monadconstraint.h"

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input (using timedInput)
--
-- Ignores buffered input lines (ie if input line gotten in under 5ms)
prompt :: MONADCONSTRAINT => String -> m String
prompt = runPrompt . timedInput . getPromptLine

-- FIXME non-empty?
-- | reads string with initial input (using timedInput)
promptInitial :: MONADCONSTRAINT => String -> String -> m String
promptInitial s = runPrompt . timedInput . getPromptInitial s

-- | reads string with buffering
promptBuffered :: MONADCONSTRAINT => String -> m String
promptBuffered = runPrompt . getPromptLine

-- | reads non-empty string (using nonEmptyInput)
promptNonEmpty :: MONADCONSTRAINT => String -> m String
promptNonEmpty = runPrompt . nonEmptyInput . getPromptLine

-- | prompt for a password
promptPassword :: MONADCONSTRAINT => String -> m String
promptPassword = runPrompt . nonEmptyInput . getPromptPassword

-- | prompt for character key
promptChar :: MONADCONSTRAINT => String -> m Char
promptChar =
  runPrompt . timedInput . getPromptChar

-- | prompt for Enter key
promptEnter :: MONADCONSTRAINT => String -> m ()
promptEnter =
  void . runPrompt . untilInput (== "") . timedInput . getPromptLine

-- | Yes-No prompt (accepts only {y,n,yes,no} case-insensitive)
yesNo :: MONADCONSTRAINT => String -> m Bool
yesNo desc =
  runPrompt . mapInput maybeYN . getPromptLine $ desc ++ "? [y/n]"
  where
    maybeYN inp =
      case trim (lower inp) of
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing

-- | Yes-No prompt with default (uses timedInput)
yesNoDefault :: MONADCONSTRAINT => Bool -> String -> m Bool
yesNoDefault yes desc =
  runPrompt . mapInput maybeYN' . timedInput . getPromptLine $
  desc ++ "? " ++ if yes then "[Y/n]" else "[y/N]"
  where
    maybeYN' inp =
      case trim (lower inp) of
        "" -> Just yes
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing
