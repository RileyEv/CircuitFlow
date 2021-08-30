{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Network.Helper where

import           Pipeline

{-| Helper function to create a network from the given Circuit
    and then input a value and recieve the output
-}
singleInputTest
  :: forall a b c d e
   . (InitialPipes a b)
  => Circuit a b c d e
  -> HList' a b
  -> IO (Either TaskError (HList' c d))
singleInputTest circuit i = do
  n <- startNetwork circuit :: IO (BasicNetwork a b c d)
  input_ i n
  out <- output_ n
  stopNetwork n
  return out
