module SimplePrompt.Internal (
  getPromptLine,
  getPromptInitial,
  getPromptChar,
  nonEmptyInput,
  timedInput,
  runPrompt,
  MonadIO,
  MonadMask
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Fixed (showFixed)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)

import System.Console.Haskeline

-- | like getInputLine, but error if fails
getPromptLine :: (MonadIO m, MonadMask m) => String -> InputT m String
getPromptLine s =
  getInputLine (s ++ ": ") >>=
  maybe (error "could not get input line!") return

-- | like getPromptLine, but with initial input
getPromptInitial :: (MonadIO m, MonadMask m)
                 => String -> String -> InputT m String
getPromptInitial s i =
  getInputLineWithInitial (s ++ ": ") (i,"") >>=
  maybe (error "could not get input line!") return

-- | like getInputLine, but error if fails
getPromptChar :: (MonadIO m, MonadMask m) => String -> InputT m Char
getPromptChar s =
  getInputChar (s ++ ": ") >>=
  maybe (error "could not read a character") return

-- | repeat prompt until non-empty
nonEmptyInput :: (MonadIO m, MonadMask m)
              => InputT m String -> InputT m String
nonEmptyInput prompting = do
  input <- prompting
  if null input
    then nonEmptyInput prompting
    else return input

-- | run a prompt
runPrompt :: (MonadIO m, MonadMask m) => InputT m a -> m a
runPrompt =  runInputT defaultSettings

timedInput :: MonadIO m => InputT m a -> InputT m a
timedInput prompter = do
  start <- liftIO getCurrentTime
  input <- prompter
  end <- liftIO getCurrentTime
  let diff = diffUTCTime end start
  if diff < 0.005
    then do
    outputStrLn $ "ignoring buffered input: " ++
      showFixed True (nominalDiffTimeToSeconds diff) ++ " too quick"
    timedInput prompter
    else return input
