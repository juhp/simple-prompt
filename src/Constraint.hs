{-# LANGUAGE CPP, ConstraintKinds #-}

module Constraint (
  MONADCONSTRAINT,
  MonadIO,
#if MIN_VERSION_haskeline(0,8,0)
  MonadMask
#else
  MonadException
#endif
  )
where

import Control.Monad.IO.Class (MonadIO)
#if MIN_VERSION_haskeline(0,8,0)
import Control.Monad.Catch (MonadMask)
#endif

type MONADCONSTRAINT m =
#if MIN_VERSION_haskeline(0,8,0)
 (MonadIO m, MonadMask m)
#else
 MonadException m
#endif
