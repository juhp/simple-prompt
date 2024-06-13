module SimplePrompt (
  prompt,
  promptInitial,
  promptBuffered,
  promptNonEmpty,
  promptChar,
  promptKeyPress,
  promptEnter,
  promptPassword,
  yesNo,
  yesNoDefault
  ) where

import Control.Monad (void)
import Data.List.Extra (lower)
import System.Console.Haskeline (waitForAnyKey)

import SimplePrompt.Internal
import Constraint

-- FIXME use haveTerminalUI ?
-- | prompt which drops buffered input (using `clearedInput`)
--
-- Ignores buffered input lines (ie if input line gotten in under 5ms)
prompt :: MONADCONSTRAINT m => String -> m String
prompt = runPrompt . clearedInput . getPromptLine

-- FIXME non-empty?
-- | reads string with initial input (using `clearedInput`)
promptInitial :: MONADCONSTRAINT m => String -> String -> m String
promptInitial s = runPrompt . clearedInput . getPromptInitial s

-- | reads string with buffering
promptBuffered :: MONADCONSTRAINT m => String -> m String
promptBuffered = runPrompt . getPromptLine

-- | reads non-empty string (using `nonEmptyInput`)
promptNonEmpty :: MONADCONSTRAINT m => String -> m String
promptNonEmpty = runPrompt . nonEmptyInput . getPromptLine

-- | prompt for a password
promptPassword :: MONADCONSTRAINT m => String -> m String
promptPassword = runPrompt . nonEmptyInput . getPromptPassword

-- | prompt for printable character
promptChar :: MONADCONSTRAINT m => String -> m Char
promptChar =
  runPrompt . clearedInput . getPromptChar

-- | prompt for key press (returns False if Ctrl-d or EOF)
promptKeyPress :: MONADCONSTRAINT m => String -> m Bool
promptKeyPress =
  runPrompt . clearedInput . waitForAnyKey

-- | prompt for Enter key
promptEnter :: MONADCONSTRAINT m => String -> m ()
promptEnter =
  void . runPrompt . untilInput (== "") . clearedInput . getPromptLine

-- | Yes-No prompt (accepts only {y,n,yes,no} case-insensitive)
yesNo :: MONADCONSTRAINT m => String -> m Bool
yesNo desc =
  runPrompt . mapInput maybeYN . getPromptLine $ desc ++ "? [y/n]"
  where
    maybeYN inp =
      case lower inp of
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing

-- | Yes-No prompt with default (uses `clearedInput`)
yesNoDefault :: MONADCONSTRAINT m => Bool -> String -> m Bool
yesNoDefault yes desc =
  runPrompt . mapInput maybeYN' . clearedInput . getPromptLine $
  desc ++ "? " ++ if yes then "[Y/n]" else "[y/N]"
  where
    maybeYN' inp =
      case lower inp of
        "" -> Just yes
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ ->  Nothing
