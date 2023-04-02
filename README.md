# simple-prompt

A little library for commandline text prompts for user input.

- `prompt`: return a String
- `prompt_` ignore input
- `yesno` expects y/n answer

Currently assumes unix since it reads from /dev/tty for fresh stdin,
but it could probably easily be extended to work on Windows.
