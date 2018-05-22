{-# LANGUAGE GADTs, StandaloneDeriving #-}

module CombinatoricsOfPermutations.M0Introduction where

data Set t where
    Null :: Set t
    Set :: (Ord t, Show t) => {
        root :: t,
        lSet :: Set t,
        rSet :: Set t
    } -> Set t

deriving instance Eq (Set t)
deriving instance Ord (Set t)

atom :: (Ord t, Show t) => t -> Set t
atom n = Set n Null Null

infix 4 <<-
_ <<- Null = False
n <<- (Set m left right) = n == m || (n < m && n <<- left) || (n > m && n <<- right)

infixl 5 <+>
(<+>) :: (Ord t, Show t) => Set t -> t -> Set t
Null <+> n = Set n Null Null
Set m left right <+> n = Set m left' right' where
    (left', right') = case compare m n of
        LT -> (left, right <+> n)
        GT -> (left <+> n, right)
        EQ -> (left, right)

infixl 5 <->
(<->) :: (Ord t, Show t) => Set t -> t -> Set t
Null <-> n = Null
(Set m left right) <-> n
    | m < n = Set m left (right <-> n)
    | m > n = Set m (left <-> n) right
    | otherwise = left <> right

infixl 5 <>
(<>) :: (Ord t, Show t) => Set t -> Set t -> Set t
Null <> set = set
set <> Null = set

(Set m left1 right1) <> (Set n left2 right2) = Set m left' right' <> res where
    (left', right', res) = case compare m n of
        LT -> (left1, right1 <> right2 <+> n, left2)
        GT -> (left1 <> left2 <+> n, right1, right2)
        EQ -> (left1 <> left2, right1 <> right2, Null)

instance Show (Set t) where
    show set = "{" ++ show' set ++ "}" where
        show' Null = ""
        show' (Set n left right) = show' left ++ comma left ++ show n ++ comma right ++ show' right where
            comma Null = ""
            comma _ = ","

setMin :: (Ord t, Show t) => Set t -> Maybe t
setMin Null = Nothing
setMin (Set n left _) = case setMin left of
    Just m -> Just m
    Nothing -> Just n

listToSet :: (Ord t, Show t) => [t] -> Set t
listToSet [] = Null
listToSet (n:ns) = Set n Null Null <> listToSet ns

setToList :: (Ord t, Show t) => Set t -> [t]
setToList Null = []
setToList (Set n left right) = n : (setToList $ left <> right)

setMap :: (Ord a, Show a, Ord b, Show b) => (a -> b) -> Set a -> Set b
setMap _ Null = Null
setMap f (Set n left right) = Set (f n) (setMap f left) (setMap f right)

consInts :: Int -> Int -> Set Int
consInts a b
    | b < a = Null
    | otherwise = Set m (consInts a (m - 1)) (consInts (m + 1) b) where
        m = div (a + b) 2

finNats :: Int -> Set Int
finNats = consInts 1

nats :: Set Int
nats = Set 1 Null (setMap (+1) nats)

newtype Perm t = Perm [t] deriving (Eq, Ord, Show)

perm :: (Ord t, Show t) => [t] -> Perm t
perm xs = Perm $ rperm Null xs where
    rperm _ [] = []
    rperm set (x:xs)
        | x <<- set = rperm set xs
        | otherwise = x : rperm (set <+> x) xs

permToList (Perm ps) = ps