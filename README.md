# simple-prompt

A simple commandline text prompt library for user input.

The `SimplePrompt` module provides:

- `prompt`: returns a string
- `promptNonEmpty` prompts for non-empty string
- `promptInitial` with pre-filled initial input
- `promptPassword` prompts for password
- `promptChar` prompts for a character
- `promptKeyPress` waits for a key press
- `promptEnter` waits for Enter key
- `yesNo` expects y/n answer
- `yesNoDefault` [y/N] or [Y/n]

It uses haskeline to read the input.

The `SimplePrompt.Internal` module provides lower-level access to
functional haskeline InputT monad transformer-based prompt functions:

- `runPrompt`, `getPrompt*`
- `untilInput`, `mapInput`, `clearedInput`, `nonEmptyInput`.
