{-# LANGUAGE GADTs, StandaloneDeriving, ViewPatterns #-}

module M1LinearOrders where

import M0Introduction

dList :: (Ord t, Integral a) => Perm t -> [a]
dList (Perm ps) = [i | ((i,p_i), (j,p_j)) <- zipped, p_i > p_j] where
    zipped = zip indexedps (tail indexedps)
    indexedps = zip [1..] ps

dSet :: (Ord t, Integral a) => Perm t -> Set a
dSet = listToSet . dList

dNum :: (Ord t, Integral a) => Perm t -> a
dNum = card . dSet