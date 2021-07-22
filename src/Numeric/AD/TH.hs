{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.TH where

import System.IO.Unsafe (unsafePerformIO)

-- containers
import qualified Data.Map as M (Map, insert, singleton)
-- -- haskell-src-meta
-- import Language.Haskell.Meta.Parse (parseDecs, parseExp)
-- template-hasekll
import Language.Haskell.TH (runQ, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Pat(..), Name(..), Quasi(..), Q, OccName, NameFlavour, Lift(..))


-- | Quasiquoter for expressions
--
-- >>> :t [decode| \x -> sin x |]
-- Double -> Double
--
-- >>> :t [decode| (,) <$> Just 3 <*> Just 'x' |]
-- Maybe (Integer, Char)
--
-- >>> :t [decode| length |]
-- Foldable t => t a -> Int
decode :: QuasiQuoter
decode = QuasiQuoter qexp qpat qty qdec
  where
    qexp _ = error "decode : undefined"
    -- qexp t = case parseExp t of
    --   Left e -> error $ unwords ["decode :", e]
    --   Right ast -> do
    --     runIO $ print ast -- FIXME DEBUG
    --     pure ast
    qpat _ = error "decode : cannot be used for patterns"
    qty _ = error "decode : cannot be used for type signatures"
    qdec _ = error "decode : cannot be used for declarations"

{- | Note :

* the expression quasiquoter [| .. |] produces an Exp in the Q monad

λ> :t  [| \x -> x + sin x |]
[| \x -> x + sin x |] :: Q Exp

* our quasiquoter :

λ> :t  [decode| \x -> x + sin x |]
[decode| \x -> x + sin x |] :: Floating a => a -> a
-}


data UOp = ULog
         | UExp
data BOp = BSum
         | BTimes
         | BDiv
         | BMinus
data Expr a = EBin BOp (Expr a) (Expr a)
            | EUn UOp (Expr a)

-- data Expr a =
--   ESum (Expr a) (Expr a)
--   | ETimes (Expr a) (Expr a)
