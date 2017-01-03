-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- # LANGUAGE StandaloneDeriving #
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

import System.IO

import SubHask
import SubHask.Algebra.Metric
import SubHask.Algebra.Array
import SubHask.Algebra.Vector
import SubHask.Compatibility.ByteString
-- import SubHask.Category.Trans.Derivative

-- import HLearn.History
-- import HLearn.Optimization.Univariate

-- import System.IO

import qualified Prelude as P
import Control.Monad.Random
import Control.Monad.State
import SubHask.Compatibility.ByteString
import SubHask.Algebra.Container
import SubHask.Algebra.Logic
import SubHask.Algebra
import SubHask.Category
-- import SubHask.Internal.Prelude
import Control.Monad
-- import Control.Monad.Trans.State
import qualified Data.List as L
import qualified Data.Vector as V
import System.IO
import HLearn.Data.SpaceTree

--------------------------------------------------------------------------------


-- instance Metric DNA

data BP = A | T | C | G |
    N | Y | S | K | M | W | R | B |
    D | H | V | U deriving (Show)

type instance Logic BP = Bool

instance Eq_ BP where
    A==A = True
    T==T = True
    C==C = True
    G==G = True
    N==_ = True
    U==_ = False
    K==x = x==K || x==G || x==T
    S==x = x==S || x==G || x==C
    Y==x = x==Y || x==T || x==C
    M==x = x==M || x==A || x==C
    W==x = x==W || x==A || x==T
    R==x = x==R || x==A || x==G
    B==x = x==B || x==T || x==G || x==C
    D==x = x==D || x==T || x==G || x==A
    H==x = x==H || x==T || x==C || x==A
    V==x = x==V || x==G || x==C || x==A
    _==_ = False

-- instance Enum BP where
--     {-# INLINE succ #-}
--     succ = P.succ

--     {-# INLINE toEnum #-}
--     toEnum i = if i < 0
--         then P.toEnum 0
--         else P.toEnum i

intToBP :: Int -> BP
intToBP 0 = A
intToBP 1 = T
intToBP 2 = C
intToBP 3 = G

bpToInt :: BP -> Int
bpToInt A = 0
bpToInt T = 1
bpToInt C = 2
bpToInt G = 3

mapToBP :: RandomGen g => (Int, g) -> (BP, g) 
mapToBP (x, ng) = (intToBP x, ng)

instance Random BP where
    -- randomR r g = randomR r g
    randomR (a,b) g = mapToBP $ randomR (bpToInt a, bpToInt b) g

    -- NOTE the 15 is hardwired and needs to match BP
    random g = mapToBP $ randomR (0,15) g

data SeqData = SeqData V.Vector BP
    deriving (Show)

randomSeq :: Int -> State StdGen [BP]
randomSeq n = Control.Monad.replicateM n randomBP
    where
        -- only ATCG
        randomBP = state $ randomR (A,G)


randomSeqs :: Int -> Int -> State StdGen [SeqData]
randomSeqs len size = Control.Monad.replicateM size (randomSeq len)
 
--------------------------------------------------------------------------------

main = do
    -- putStrLn $ show (distance a b)

    g <- getStdGen
    let a = evalState (randomSeqs 5 10) g
    putStrLn $ show $  a
