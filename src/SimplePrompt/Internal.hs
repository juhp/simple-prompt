{-# LANGUAGE CPP #-}

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
  timedInput,
  MonadIO,
#if MIN_VERSION_haskeline(0,8,0)
  MonadMask
#else
  MonadException
#endif
  ) where

#if MIN_VERSION_haskeline(0,8,0)
import Control.Monad.Catch (MonadMask)
#endif
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import System.Console.Haskeline

#include "../monadconstraint.h"

-- | generic prompt wrapper
getGenericPrompt :: MonadIO m => (String -> InputT m (Maybe a))
                 -> String -> InputT m a
getGenericPrompt prompter s =
  prompter (s ++ ": ") >>=
  maybe (error "could not read input!") return

-- | like getInputLine, but error if fails
getPromptLine :: MONADCONSTRAINT => String -> InputT m String
getPromptLine =
  getGenericPrompt getInputLine

-- | like getPromptLine, but with initial input
getPromptInitial :: MONADCONSTRAINT => String -> String -> InputT m String
getPromptInitial s i =
  getGenericPrompt (`getInputLineWithInitial` (i,"")) s

-- | like getInputChar, but error if fails
getPromptChar :: MONADCONSTRAINT => String -> InputT m Char
getPromptChar =
  getGenericPrompt getInputChar

-- | get password
getPromptPassword :: MONADCONSTRAINT => String -> InputT m String
getPromptPassword =
  getGenericPrompt (getPassword Nothing)

-- | run a prompt
runPrompt :: MONADCONSTRAINT => InputT m a -> m a
runPrompt =  runInputT defaultSettings

-- | loop prompt until check
untilInput :: MONADCONSTRAINT => (a -> Bool) -> InputT m a -> InputT m a
untilInput p prompting = do
  input <- prompting
  if p input
    then return input
    else untilInput p prompting

-- | maybe map input or loop prompt
mapInput :: MONADCONSTRAINT => (a -> Maybe b) -> InputT m a -> InputT m b
mapInput f prompting = do
  input <- prompting
  case f input of
    Just x -> return x
    Nothing -> mapInput f prompting

-- | repeat prompt until non-empty
nonEmptyInput :: MONADCONSTRAINT => InputT m String -> InputT m String
nonEmptyInput = untilInput (not . null)

-- | repeat prompt if input returned within milliseconds
timedInput :: MonadIO m => InputT m a -> InputT m a
timedInput prompter = do
  start <- liftIO getCurrentTime
  input <- prompter
  end <- liftIO getCurrentTime
  let diff = diffUTCTime end start
  if diff < 0.005
    then do
    outputStrLn $ "ignoring buffered input: " ++ show diff ++ " too quick"
    timedInput prompter
    else return input
