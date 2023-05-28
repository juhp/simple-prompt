module SimplePrompt.Internal (
  getPromptOrError,
  readNonEmpty,
  readClear,
  runPrompt
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Fixed (showFixed)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)

import System.Console.Haskeline

getPromptOrError :: String -> InputT IO String
getPromptOrError s =
  getInputLine (s ++ ": ") >>=
  maybe (error "could not get input line!") return

-- | does a non-empty prompt
readNonEmpty :: IO String -> IO String
readNonEmpty prompting = do
  input <- prompting
  if null input
    then readNonEmpty prompting
    else return input

-- | run a prompt
runPrompt :: InputT IO String -> IO String
runPrompt prompter =
  runInputT defaultSettings prompter

readClear :: String -> InputT IO String
readClear s = do
  start <- liftIO getCurrentTime
  input <- getPromptOrError s
  end <- liftIO getCurrentTime
  let diff = diffUTCTime end start
  if diff < 0.005
    then do
    outputStrLn $ "ignoring buffered input: " ++
      showFixed True (nominalDiffTimeToSeconds diff) ++ " too quick"
    readClear s
    else return input
