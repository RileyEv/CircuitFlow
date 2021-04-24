{-|
Module      : Pipeline.Network
Description : To create and control process networks
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

Contains the tools needed to create, interact with and stop a network.
-}
module Pipeline.Network
  (
  -- * Network
    Network(..)
  ,
  -- * Basic Network
    BasicNetwork
  ,
  -- * Network IO
    UUID
  ,
  -- ** Input
    input
  , input_
  ,
  -- ** Output
    output_
  , module Pipeline.Internal.Common.HList
  ,
  -- * Misc
    InitialPipes
  ) where

import           Pipeline.Internal.Backend.BasicNetwork (BasicNetwork)
import           Pipeline.Internal.Backend.Network      (InitialPipes,
                                                         Network (..))
import           Pipeline.Internal.Common.HList         (HList' (..))
import           Pipeline.Internal.Core.CircuitAST      (Circuit)
import           Pipeline.Internal.Core.Error           (TaskError)
import           Pipeline.Internal.Core.UUID            (UUID, genUUID)
import           Prelude                                hiding (read)


-- | Input values into a network.
-- This will return a randomly generated identifier for the inputs.
input
  :: Network n
  => HList' inputsS inputsT -- ^ Inputs to the network
  -> n inputsS inputsT inputsA outputsS outputsT outputsA -- ^ Network to insert the values in
  -> IO UUID -- ^ Randomly generated identifier
input x n = do
  uuid <- genUUID
  write uuid x n
  return uuid


-- | A variant of 'input', however it will not return the randomly generated identifier.
input_
  :: HList' inputsS inputsT
  -> BasicNetwork inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO ()
input_ x n = do
  _ <- input x n
  return ()


-- | A variant of 'output', that does not return the unique identifier.
--
--   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
output_
  :: Network n
  => n inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO (Either TaskError (HList' outputsS outputsT))
output_ n = read n >>= (\(_, x) -> return x)
