# Revision history for simple-prompt

## 0.2.0 (2023-05-28)
- new API using haskeline and MonadIO
- prompt ignores buffered stdin lines if it returns in milliseconds
- promptEnter replaces prompt_
- new functions: promptEnter, promptInitial, promptNonEmpty, promptPassword
- yesNo and yesNoDefault replace yesno
- internal haskeline functions in SimplePrompt.Internal: including
  getPrompt*, runPrompt, untilInput, mapInput, nonEmptyInput, timedInput

## 0.1.0 (2023-04-02)
- initial release with prompt, prompt_, yesno functions
