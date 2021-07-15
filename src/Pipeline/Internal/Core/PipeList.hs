module Pipeline.Internal.Core.PipeList
  ( PipeList(..)
  , AppendP(..)
  , takeP
  , dropP
  ) where

import           Control.Concurrent.Chan           (Chan)
import           Data.Kind                         (Type)
import           Pipeline.Internal.Common.Nat      (SNat (..))
import           Pipeline.Internal.Common.TypeList (Apply, Drop, Take, (:++))
import           Pipeline.Internal.Core.Error      (TaskError)
import           Pipeline.Internal.Core.UUID       (UUID)


data PipeList (fs :: [Type -> Type]) (as :: [Type]) (xs :: [Type]) where
  PipeCons ::(Eq (f a)) => Chan (UUID, Either TaskError (f a)) -> PipeList fs as xs -> PipeList (f ': fs) (a ': as) (f a ': xs)
  PipeNil ::PipeList '[] '[] (Apply '[] '[])


takeP :: SNat n -> PipeList fs as xs -> PipeList (Take n fs) (Take n as) (Take n xs)
takeP SZero     _               = PipeNil
takeP (SSucc _) PipeNil         = PipeNil
takeP (SSucc n) (PipeCons x xs) = PipeCons x (takeP n xs)


dropP :: SNat n -> PipeList fs as xs -> PipeList (Drop n fs) (Drop n as) (Drop n xs)
dropP SZero     l               = l
dropP (SSucc _) PipeNil         = PipeNil
dropP (SSucc n) (PipeCons _ xs) = dropP n xs


class AppendP fs as xs gs bs ys where
  appendP :: PipeList fs as xs -> PipeList gs bs ys -> PipeList (fs :++ gs) (as :++ bs) (xs :++ ys)

instance AppendP '[] '[] '[] gs bs ys where
  appendP PipeNil ys = ys

instance (AppendP fs as xs gs bs ys) => AppendP (f ': fs) (a ': as) (f a ': xs) gs bs ys where
  appendP (PipeCons x xs) ys = PipeCons x (appendP xs ys)
