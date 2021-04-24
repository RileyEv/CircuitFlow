{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Network.Helper where

import           Pipeline

{-| Helper function to create a network from the given Circuit
    and then input a value and recieve the output
-}
singleInputTest
  :: forall a b c d e f g
   . (InitialPipes a b c)
  => Circuit a b c d e f g
  -> HList' a b
  -> IO (Either TaskError (HList' d e))
singleInputTest circuit i = do
  n <- startNetwork circuit :: IO (BasicNetwork a b c d e f)
  input_ i n
  out <- output_ n
  stopNetwork n
  return out
