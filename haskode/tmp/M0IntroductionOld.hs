{-# LANGUAGE GADTs #-}

--module CombinatoricsOfPermutations.M0Introduction where

-- Without GADTs
data Set t = Null | Set t (Set t) (Set t)
    -- Update 1 (GADT)
    data Set t where
        Null :: Set t
        Set :: (Ord t, Show t) => {
            root :: t,
            lSet :: Set t,
            rSet :: Set t
        } -> Set t

infix 5 <+>
(<+>) :: (Ord t, Show t) => Set t -> t -> Set t
Null <+> n = Set n Null Null
Set m left right <+> n
    | m < n = Set m left (right <+> n)
    | m > n = Set m (left <+> n) right
    | otherwise = Set m left right

-- Exhaustive show
instance Show (Set t) where
    show set = "{" ++ show' set ++ "}" where
        show' Null = ""
        show' (Set n Null Null) = show n
        show' (Set n Null right) = show n ++ "," ++ show' right
        show' (Set n left Null) = show' left ++ "," ++ show n
        show' (Set n left right) = show' left ++ "," ++ show n ++ "," ++ show' right 
    -- Update 1 (Smarter show)
    instance Show (Set t) where
        show set = "{" ++ show' set ++ "}" where
            show' Null = ""
            show' (Set n left right) = show' left ++ comma left ++ show n ++ comma right ++ show' right where
                comma Null = ""
                comma _ = ","       

consInts :: Int -> Int -> Set Int
consInts a b
    | b < a = Null
    | otherwise = Set m (consInts a (m - 1)) (consInts (m + 1) b) where
        m = div (a + b) 2

natSet = consInts 1