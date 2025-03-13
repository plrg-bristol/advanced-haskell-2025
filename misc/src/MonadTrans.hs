{-# language FlexibleContexts #-}
module MonadTrans where

import Control.Monad.Except
import Control.Monad.IO.Class
    -- more than one monad at once
    -- MT allow more than one monad at once
    -- there are two ways of doing this
    --   * algebraic effects
            -- list ~ non det
            -- Maybe ~ failure
            -- State ~ state
    --   * Monad Trans
    --     -- framework for composing together different monads

eg1 :: [Maybe Int] -> [Maybe Int]
eg1 xs = fmap (fmap (+ 10)) xs

-- eg2 :: [Maybe Int] -> [Maybe Int] -> [Int]
-- eg2 xs ys = do
--     x <- xs

-- eg3 :: (Monad m, Monad f) => m (f Int) -> m (f Int)
-- eg3 xs = do

-- eg4 :: Either (IO a) String
-- eg4 = Left $ 

-- data ExceptIO e a = ExceptIO (IO (Either e a))
-- data ExceptT m e a = ExceptT (m (Either e a))

-- type ExceptIO' = ExceptT IO

egErr :: MonadError String m => m Int
egErr = do
    -- throwError "This is an error"
    return 0

egErrIO
  :: (MonadError String m, MonadIO m) => m ()
egErrIO = do
    x <- egErr
    liftIO (print x)
    pure ()