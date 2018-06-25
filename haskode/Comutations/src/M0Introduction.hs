{-# LANGUAGE GADTs, StandaloneDeriving, PatternSynonyms #-}

module M0Introduction (Set(Null), pattern Set, (<<=), (<<), (<<-), (->>), (<+>), (<**>),
                        setMap, card, setFoldl, setFoldr, setZipWith, setMin, setMax, listToSet, setToList, setToList', 
                        atom, consInts, finNats, nats,
                        Perm, pattern Perm, perm, pList, permIns, revPerm, addPerms, perms, permSet) where

data Set t where
    Null :: Set t    
    NESet :: Ord t => {
        root :: t,
        lSet :: Set t,   
        rSet :: Set t
    } -> Set t

pattern Set n left right <- NESet n left right

instance Show t => Show (Set t) where
    show set = "{" ++ show' set ++ "}" where
        show' Null = ""
        show' (Set n left right) = show' left ++ comma left ++ show n ++ comma right ++ show' right where
            comma Null = ""
            comma _ = ","

-- Set membership
infix 4 <<=
(<<=) :: t -> Set t -> Bool
_ <<= Null = False
n <<= (Set m left right) = n == m || (n < m && n <<= left) || (n > m && n <<= right)

-- Subset
infix 4 <<
(<<) :: Set t -> Set t -> Bool
Null << _ = True
Set n left right << mset = n <<= mset && left << mset && right << mset

-- Set equality
instance Eq (Set t) where
    mset == nset = mset << nset && nset << mset

deriving instance Ord (Set t)

-- Insert into set
infixl 5 <<-
(<<-) :: Ord t => Set t -> t -> Set t
Null <<- n = NESet n Null Null
Set m left right <<- n = NESet m left' right' where
    (left', right') = case compare m n of
        LT -> (left, right <<- n)
        GT -> (left <<- n, right)
        EQ -> (left, right)

-- Delete from set
infixl 5 ->>
(->>) :: Set t -> t -> Set t
Null ->> n = Null
(Set m left right) ->> n
    | m < n = NESet m left (right ->> n)
    | m > n = NESet m (left ->> n) right
    | otherwise = left <+> right

-- Set union
infixl 5 <+>
(<+>) :: Set t -> Set t -> Set t
Null <+> set = set
set <+> Null = set
(Set m left1 right1) <+> (Set n left2 right2) = NESet m left' right' <+> res where
    (left', right', res) = case compare m n of
        LT -> (left1, right1 <+> right2 <<- n, left2)
        GT -> (left1 <+> left2 <<- n, right1, right2)
        EQ -> (left1 <+> left2, right1 <+> right2, Null)

-- Set transformation by unary function applied to all elements
setMap :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
setMap _ Null = Null
setMap f (Set n left right) = NESet (f n) (setMap f left) (setMap f right)

-- Set Cartesian product
infixl 6 <**>
(<**>) :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
Null <**> _ = Null
Set m left right <**> nset = setMap (\n -> (m, n)) nset <+> left <**> nset <+> right <**> nset

-- Set cardinality
card :: Integral i => Set t -> i
card Null = 0
card (Set n left right) = 1 + card left + card right

-- Set left fold
setFoldl :: (a -> t -> a) -> a -> Set t -> a
setFoldl _ seed Null = seed
setFoldl f seed (Set n left right) = setFoldl f (setFoldl f seed left `f` n) right

-- Set right fold
setFoldr :: (t -> a -> a) -> a -> Set t -> a
setFoldr _ seed Null = seed
setFoldr f seed (Set n left right) = setFoldr f (n `f` setFoldr f seed right) left

-- Set Cartesian product with binary operation applied to tuples
setZipWith :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> Set a -> Set b -> Set c
setZipWith op mset nset = setMap (\(m,n) -> op m n) $ mset <**> nset

-- Least element of a set
setMin :: Set t -> Maybe t
setMin Null = Nothing
setMin (Set n left _) = case setMin left of
    Just m -> Just m
    Nothing -> Just n

-- Greatest element of a set
setMax :: Set t -> Maybe t
setMax Null = Nothing
setMax (Set n _ right) = case setMax right of
    Just m -> Just m
    Nothing -> Just n

-- List to set conversion
listToSet :: Ord t => [t] -> Set t
listToSet = foldr (\n set -> set <<- n) Null

-- Set to list conversion using right fold
setToList :: Set t -> [t]
setToList = setFoldr (:) []

-- Set to list conversion using left fold
setToList' :: Set t -> [t]
setToList' = setFoldl (\list n -> n:list) []

-- Singleton set
atom :: Ord t => t -> Set t
atom n = NESet n Null Null

-- Set of consecutive integers
consInts :: Integral i => i -> i -> Set i
consInts a b
    | b < a = Null
    | otherwise = NESet m (consInts a (m - 1)) (consInts (m + 1) b) where
        m = div (a + b) 2

-- Set of first n naturals
finNats :: Integral i => i -> Set i
finNats = consInts 1

-- Set of all naturals
nats :: Integral i => Set i
nats = NESet 1 Null (setMap (+1) nats)

newtype Perm t = Permutation {pList :: [t]} deriving (Eq, Ord)

pattern Perm ps <- Permutation ps

-- Insert into permutation
permIns :: Eq t => Perm t -> t -> Perm t
permIns (Permutation ps) n
    | n `elem` ps = Permutation ps
    | otherwise = Permutation (n:ps)

-- Safe permutation constructor
perm :: Eq t => [t] -> Perm t
perm = Permutation . reverse . pList . foldl permIns (Permutation [])

setIns Null n = (NESet n Null Null, True)
setIns (Set m l r) n
    | m < n = let (r', succ) = setIns r n in (NESet m l r', succ)
    | m > n = let (l', succ) = setIns l n in (NESet m l' r, succ)
    | otherwise = (NESet m l r, False)

instance Show t => Show (Perm t) where
    show = show . pList

-- Reverse of a permutation
revPerm :: Eq t => Perm t -> Perm t
revPerm = Permutation . reverse . pList

-- Safe concatenation of permutations
addPerms :: Eq t => Perm t -> Perm t -> Perm t
addPerms (Perm ps) qperm = foldl (permIns) qperm $ reverse ps

-- List of all permutations of a set
perms :: Ord t => Set t -> [Perm t]
perms = map perm . perms' [[]] . setToList where
    perms' ps [] = ps
    perms' ps (n:ns) = perms' [first ++ [n] ++ rest | (first, rest) <- concat $ map lisa ps] ns
    lisa [] = [([], [])]
    lisa (p:ps) = ([], p:ps) : [(p:first, rest) | (first, rest) <- lisa ps]

-- Set of all permutations of a set
permSet :: Ord t => (Set t) -> Set (Perm t)
permSet = listToSet . perms