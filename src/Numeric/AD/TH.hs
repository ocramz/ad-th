{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.TH where

-- haskell-src-meta
import Language.Haskell.Meta.Parse (parseDecs, parseExp)
-- template-hasekll
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Name(..), Quasi(..), Q)


decode :: QuasiQuoter
decode = QuasiQuoter qexp qpat qty qdec
  where
    qexp t = case parseExp t of
      Left e -> error $ unwords ["decode :", e]
      Right x -> pure x
    qpat _ = error "decode : cannot be used for patterns"
    qty _ = error "decode : cannot be used for type signatures"
    qdec _ = error "decode : cannot be used for declarations"



