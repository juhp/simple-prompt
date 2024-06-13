module SimplePrompt.Internal (
  getPromptLine,
  getPromptInitial,
  getPromptChar,
  getPromptPassword,
  getGenericPrompt,
  runPrompt,
  untilInput,
  mapInput,
  nonEmptyInput,
  clearedInput
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Safe (lastMay)

import System.Console.Haskeline
import Constraint

-- | generic prompt wrapper
getGenericPrompt :: MonadIO m => (String -> InputT m (Maybe a))
                 -> String -> InputT m a
getGenericPrompt prompter s =
  let suff =
        case lastMay s of
          Just '\n' -> ""
          Just ':' -> " "
          _ -> ": "
  in
  prompter (s ++ suff) >>=
  maybe (error "could not read input!") return

-- | like getInputLine, but error if fails
getPromptLine :: MONADCONSTRAINT m => String -> InputT m String
getPromptLine =
  getGenericPrompt getInputLine

-- | like getPromptLine, but with initial input
getPromptInitial :: MONADCONSTRAINT m => String -> String -> InputT m String
getPromptInitial s i =
  getGenericPrompt (`getInputLineWithInitial` (i,"")) s

-- | like getInputChar, but error if fails
getPromptChar :: MONADCONSTRAINT m => String -> InputT m Char
getPromptChar =
  getGenericPrompt getInputChar

-- | get password
getPromptPassword :: MONADCONSTRAINT m => String -> InputT m String
getPromptPassword =
  getGenericPrompt (getPassword Nothing)

-- | run a prompt
runPrompt :: MONADCONSTRAINT m => InputT m a -> m a
runPrompt =  runInputT defaultSettings

-- | loop prompt until check
untilInput :: MONADCONSTRAINT m => (a -> Bool) -> InputT m a -> InputT m a
untilInput p prompting = do
  input <- prompting
  if p input
    then return input
    else untilInput p prompting

-- | maybe map input or loop prompt
mapInput :: MONADCONSTRAINT m => (a -> Maybe b) -> InputT m a -> InputT m b
mapInput f prompting = do
  input <- prompting
  case f input of
    Just x -> return x
    Nothing -> mapInput f prompting

-- | repeat prompt until non-empty
nonEmptyInput :: MONADCONSTRAINT m => InputT m String -> InputT m String
nonEmptyInput = untilInput (not . null)

-- | repeat prompt if input returned within milliseconds
-- This prevents buffered stdin lines from being used.
clearedInput :: MonadIO m => InputT m a -> InputT m a
clearedInput prompter = do
  start <- liftIO getCurrentTime
  input <- prompter
  end <- liftIO getCurrentTime
  let diff = diffUTCTime end start
  if diff < 0.005
    then do
    outputStrLn $ "ignoring buffered input: " ++ show diff ++ " too quick"
    clearedInput prompter
    else return input
