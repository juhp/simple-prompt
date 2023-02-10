module SimplePrompt (
  prompt,
  prompt_,
  yesno
  ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isPrint)
import Data.List.Extra (lower, trim)

import System.IO

prompt :: String -> IO String
prompt s = do
  putStr $ s ++ ": "
  tty <- openFile "/dev/tty" ReadMode
  inp <- hGetLine tty
  if all isPrint inp
    then return inp
    else do
    putStrLn $
      "input rejected because of unprintable character(s): '" ++
      show inp ++ "'"
    prompt s

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

yesno :: Maybe Bool -> String -> IO Bool
yesno mdefault desc = do
  inp <- prompt $ desc ++ "? " ++ maybe "[y/n]" (bool "[y/N]" "[Y/n]") mdefault
  case trim (lower inp) of
    "y" -> return True
    "yes" -> return True
    "n" -> return False
    "no" -> return False
    "" -> maybe (yesno Nothing desc) return mdefault
    _ ->  yesno mdefault desc
