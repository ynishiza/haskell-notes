-- Run with 
--  stack exec -- src/scratch/<name>.hs
--  stack ghci -- src/scratch/<name>.hs
{-# LANGUAGE GADTs #-}

import Control.Monad
import qualified Control.Monad.Trans.Cont as C

data Cont r a where
  Cont :: { runCont :: (a -> r) -> r }-> Cont r a

instance Functor (Cont r) where
  fmap fn (Cont f) = Cont $ \next -> f (next. fn)

instance Applicative (Cont r) where
  pure x = Cont $ \next -> next x
  (<*>) = ap

instance Monad (Cont r) where
  (Cont f) >>= k = Cont $ \next -> f (\a -> runCont (k a) next)

bypass :: r -> Cont r a
bypass r = Cont $ const r

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \next -> runCont (f (bypass . next)) next

reset :: Cont a a -> Cont r a
reset (Cont next) = pure $ next id

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f  = Cont $ evalCont . f

evalCont :: Cont r r -> r
evalCont (Cont f) = f id

test :: IO ()
test = do
  let 
    compWithCC :: Cont r String
    compWithCC = do
      x <- callCC (\next -> do
        _ <- next "ERROR"
        -- bypass
        return "SUCCESS"
        )
      return $ "Result:" <> x

    compWithLoop :: Cont r String
    compWithLoop = do
      x <- reset (do
        y <- shift (\next -> pure $ next (next (next "a")))
        return $ "[" <> y <> "]" 
        )
      return $ "Result=  " <> x

    compWithLoop_ :: C.Cont r String
    compWithLoop_ = do
      x <- C.reset (do
        y <- C.shift (\next -> pure $ next (next (next "a")))
        return $ y <> "," <> y  <> ":"
        )
      return $ "Result  " <> x

    comp3 :: Cont r String
    comp3 = do
      x <- reset (do
        y <- shift (\next -> pure $ 
            "\n" <> wrap "*" (next $  
              "\n  " <> wrap "@" (next $ 
                "\n   " <> wrap "#"  (next "a"))))
        z <- shift (\next -> pure $ next $ wrap "$" (next "b"))
        return $ "[" <> y <> "," <> z <> "]" 
        )
      return $ "Result=  " <> x
        where 
          wrap s x = s <> x <> s

    comp3' :: C.Cont r String
    comp3' = do
      x <- C.reset (do
        y <- C.shift (\next -> pure $ "\n" <> wrap "1" (
              next $  "\n  " <> wrap "2" (
                next $ "\n   " <> wrap "3"  (
                  next "a"))))
        z <- C.shift (\next -> pure $ next $ "$" <> next "b")
        return $ "[" <> y <> "," <> z <> "]" 
        )
      return $ "Result=  " <> x
        where 
          wrap s x = s <> x <> s

    comp4 :: Cont r String
    comp4 = do
      x <- reset (do
        y <- shift (\next -> pure $ 
            "\n" <> wrap "*" (next $
              "\n " <> wrap "_" (next "a")))
        z <- shift (\next -> pure $ 
          next (wrap "#" (next "b")))
        return $ "[" <> y <> "," <> z <> "]" 
        )
      return $ "Result=  " <> x
        where 
          wrap s x = s <> x <> s


    comp5 :: Int -> Int -> Cont r String
    comp5 x0 y0 = do
      y <- reset (do
        a <- shift (\next -> pure $ next (next x0))
        b <- shift (\next -> pure $ next (next y0))
        return $ 2 * a + b)
      return $ "Result:" <> show (y + 1)

  putStrLn "=== My Cont === "
  putStrLn $ "callCC:\t" <> evalCont compWithCC
  putStrLn $ "delimit:\t" <> evalCont compWithLoop
  putStrLn $ "delimit:\t" <> evalCont comp3
  putStrLn $ "delimit:\t" <> evalCont comp4
  putStrLn $ "delimit:\t" <>  evalCont (comp5 1 1)
  putStrLn $ "delimit:\t" <>  evalCont (comp5 1 2)

  putStrLn "=== Real Cont === "
  putStrLn $ "delimit:\t" <> C.evalCont compWithLoop_
  putStrLn $ "delimit:\t" <> C.evalCont comp3'
  pure ()
