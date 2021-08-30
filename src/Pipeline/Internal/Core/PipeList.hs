module Pipeline.Internal.Core.PipeList
  ( PipeList(..)
  , AppendP(..)
  , takeP
  , dropP
  ) where

import           Control.Concurrent.Chan           (Chan)
import           Data.Kind                         (Type)
import           Pipeline.Internal.Common.Nat      (SNat (..))
import           Pipeline.Internal.Common.TypeList (Drop, Take, (:++))
import           Pipeline.Internal.Core.Error      (TaskError)
import           Pipeline.Internal.Core.UUID       (JobUUID)


data PipeList (fs :: [Type -> Type]) (as :: [Type]) where
  PipeCons ::(Eq (f a))
    => Chan (JobUUID, Either TaskError (f a))
    -> PipeList fs as
    -> PipeList (f ': fs) (a ': as)
  PipeNil ::PipeList '[] '[]


takeP :: SNat n -> PipeList fs as -> PipeList (Take n fs) (Take n as)
takeP SZero     _               = PipeNil
takeP (SSucc _) PipeNil         = PipeNil
takeP (SSucc n) (PipeCons x xs) = PipeCons x (takeP n xs)


dropP :: SNat n -> PipeList fs as -> PipeList (Drop n fs) (Drop n as)
dropP SZero     l               = l
dropP (SSucc _) PipeNil         = PipeNil
dropP (SSucc n) (PipeCons _ xs) = dropP n xs


class AppendP fs as gs bs where
  appendP :: PipeList fs as -> PipeList gs bs -> PipeList (fs :++ gs) (as :++ bs)

instance AppendP '[] '[] gs bs where
  appendP PipeNil ys = ys

instance (AppendP fs as gs bs) => AppendP (f ': fs) (a ': as) gs bs where
  appendP (PipeCons x xs) ys = PipeCons x (appendP xs ys)
