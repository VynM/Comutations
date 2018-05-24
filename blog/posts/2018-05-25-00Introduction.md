---
title: Introduction
---

This is a very small chapter (just one page) in the book, and all we do here is define a permutation, introduce some notation, and count the total number of permutations of a given number of objects (everyone knows the answer). Nevertheless, we need to tell Haskell what a set is before we can tell it what a permutation is. And then we can also define some of the familiar operations on sets, which may come in handy later.
<!--more-->

A permutation is an arrangement. Of elements of some set. We don\'t care what the elements are, so they might as well be the familar natural numbers that are easy to talk about. We\'ll denote the set of the first $n$ natural numbers as $[n] = \{1, 2, \ldots, n\}$. Now we can define (our own version of) what a permutation is.

<div class="def env">
<header>Definition: Permutation</header>
<section>
A *permutation* is a linear ordering of the elements of the set $[n]$. In particular, this is an *$n$-permutation*. A general $n$-permutation $p$ is written as $p_1 p_2 \cdots p_n$, where $p_i$ is the <nobr>$i$<sup>th</sup></nobr> element in the linear order defined by $p$.
</section>
</div>

There are other possible definitions of permutations, and we will be seeing those in later chapters.

Okay, so we\'ve got permutations, but where is the combinatorics? Here it is, in this well known result.

<div class="prop env"><a id="prop:n-permutations"></a>
<header>Proposition</header>
<section>
The number of $n$-permutations is $n!$.
</section>
</div>

I promise the rest of this post will be less straightforward.<sup><a title="Bóna ends the introduction here, with 'I promise the rest of the book will be less straightforward'" style="cursor:pointer;">1</a></sup>

## Sets in Haskell
To efficiently encode a set in Haskell, we will use a <a href="https://en.wikipedia.org/wiki/Binary_search_tree" title="Wikipedia – Binary Search Tree" target="_blank">binary search tree</a>. First, let us look at what a binary tree is. A binary tree can be recursively defined as a structure consisting of a node and two children (left and right), each of which is itself a binary tree. But that defines an infinite binary tree, so we add a base case to this definition by stating that a binary tree is either an empty tree, or non-empty tree as we already defined.

<div class="exm env">
<header>Example: Binary Tree</header>
<section>
<span style="display:inline-block; vertical-align: middle; line-height: 20px;"><pre>
    a
   / \
  b   c
 /\   /\
.  . x  y
</pre></span>
(where . denotes an empty tree), or equivalently
<span style="display:inline-block; vertical-align: middle; line-height: 20px;"><pre>
    a
   / \
  b   c
      /\
     x  y
</pre></span>
is a binary tree with root node \'a\', whose left child is the (singleton) binary tree with root \'b\' and right child is the tree with root \'c\'. This latter tree itself has two children with root nodes \'x\' and \'y\'.
</section>
</div>

A binary search tree is a binary tree satisfying a constraint: every element in the left child of a node must be smaller than the element at the node, and similarly, every element in the right child should be greater than the element at the node.

For the moment, let us set the contraint aside and try to define a simple binary tree in Haskell. We will call it `Set`{.haskell}, since that\'s what we\'re trying to define using trees. Our recursive definition of a binary tree (or `Set`{.haskell}) can be beautifully implemented in Haskell by means of a <a href="https://wiki.haskell.org/Type#Data_declarations" title="Data Declarations – HaskellWiki" target="_blank">data declaration</a>:\
`data Set t = Null | NonEmpty t (Set t) (Set t)`{.haskell}.\

<span class="qnote">
<a href="http://learnyouahaskell.com/making-our-own-types-and-typeclasses" title="Learn You a Haskell for Great Good!" target="_blank">Here</a> is a great tutorial on types and typeclasses, if things are starting to get confusing.
</span>

However, the constructor name `NonEmpty`{.haskell} is too unwieldly, so let\'s replace it by something shorter. And why not `Set`{.haskell} itself? This is allowed, and it causes no problems – we just have to remember which is which. One is a type and the other is a function (Exercise: Write the type of the constructor function `Set`{.haskell}), so there is no cause for confusion.

Now using <a href="https://en.wikibooks.org/wiki/Haskell/Pattern_matching" title="Pattern Matching – Haskell Wikibook" target="_blank">pattern matching</a> we can define a function that extracts the element at the root node of the tree representing a set:\
`root (Set n left right) = n`{.haskell}.\


We could similarly define functions to extract the left and right children of the tree. But this is so mechanical a process that we can think of automating it. And that has already been done! Haskell\'s *record syntax* for data declarations automatically defines these \"getter\" functions for us. So let\'s rewrite the definition of `Set`{.haskell}.
```{.haskell}
data Set t =
    Null |
    Set {
        root :: t,
        lSet :: Set t,
        rSet :: Set t
    }
```

This is a binary tree, not a binary *search* tree – because there is nothing to stop us from defining a set `Set 1 (Set 2 Null (Set 3)) (Set 4 (Set 4) (Set 5))`{.haskell}, which violates the constraint that left child elements must be smaller and right child elements must be greater than the root element (note that repeating an element is one of the ways of violating this contraint). This will break all the operations that we will be defining under the assumption that we are operating on a binary search tree. To prevent this, we will define a new way of constructing sets – adding one element at a time and making sure that it goes to its correct place in the tree.

But talking about comparing the elements of a set assumes they *are* comparable – in Haskell, this means they are members of the class `Ord`{.haskell}. Under no circumstances do we want to define or use a set of elements that cannot be ordered, so we can enforce this as a constraint in the data declaration itself. To do this, however, we need to enable the <a href="https://en.wikibooks.org/wiki/Haskell/GADT" title="Haskell Wikibook – Generalised Algebraic Datatype" target="_blank">Generalised Algebraic Datatypes</a> (GADTs) language option by adding `{-# LANGUAGE GADTs -#}`{.haskell} to the beginning of our source file. Now we rewrite `Set`{.haskell} yet again.
```{.haskell}
data Set t where
    Null :: Set t
    Set :: Ord t => t -> Set t -> Set t -> Set t
```
Remember (if you did the exercise) that the `Set`{.haskell} constructor is a function of type `t -> Set t -> Set t -> Set t`{.haskell}. Now we have just added the class constraint `Ord t` to its type signature. If we want to retain the record syntax, we have to rewrite this (one final time).
```{.haskell}
data Set t where
    Null :: Set t
    Set :: Ord t => {
        root :: t,
        lSet :: Set t,
        rSet :: Set t
    } -> Set t
```

Now let us write that function for (correctly) building sets. We could write an `insert`{.haskell} function and then to construct the set $\{1,2,3,4,5\}$, we would have to write\
`insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 5 Null`{.haskell}.\
Tiresome! (Exercise: Write the expression for the same set assuming that the *first* argument of `insert`{.haskell} is the set and the second one is the element). It\'s much cleaner to use an operator.
```{.haskell}
infixl 5 <<-
(<<-) :: Set t -> t -> Set t
```
Now `<<-`{.haskell} is a left-associative infix operator with fixity `5`, whose first argument is of type `Set t`{.haskell} and second argument is (an element) of type `t`{.haskell}. What should this operator do, for example, if we write `set <<- n`{.haskell} (where `set`{.haskell} is some already defined set)? Let\'s assume that `set = Set m left right`{.haskell} (i.e., it has root element `m`{.haskell}, and left and right children `left`{.haskell} and `right`{.haskell} respectively). If `n`{.haskell} is smaller than `m`{.haskell}, then we should insert it in the left child, and if it is greater than `m`{.haskell}, we should insert it in the right child. Otherwise, there is nothing to do, it\'s already in `set`{.haskell}. To insert `n`{.haskell} in the left or right child, we again use `<<-`{.haskell}, of course: `left <<- n`{.haskell} or `right <<- n`{.haskell}. So this function is recursive, and needs a base case. Where does our process (as it is at present) fail? When we reach a set that has no left or right child. That\'s `Null`{.haskell}. So the base case is inserting an element into `Null`{.haskell}, and that should obviously give us `Set n Null Null`{.haskell}. And that\'s it, we\'re done! Using pattern matching:
```{.haskell}
Null <<- n = Set n Null Null
Set m left right <<- n = Set m left' right' where
    (left', right') = case compare m n of
        LT -> (left, right <<- n)
        GT -> (left <<- n, right)
        EQ -> (left, right)
```
Now we can write our earlier example as `Null <<- 1 <<- 2 <<- 3 <<- 4 <<- 5`{.haskell}. So much simpler! But we can\'t \"see\" this set because we haven\'t written a `show`{.haskell} function yet. We could derive `Show`{.haskell}, but that\'s not of much use if we want the set to be displayed as `{1,2,3,4,5}`{.haskell} (like in math). So we need a clever `show`{.haskell} function.

It must be recursive and the base case is clearly `show Null = "{}"`{.haskell}. Or is it? Remember that a non-empty set can have nodes with `Null`{.haskell} children. We don\'t want these as well to be shown as `"{}"`{.haskell}. And in a recursive function call, there is no way of telling (without using flags or something similarly ugly), whether the current `Null`{.haskell} set is a child of some node or not. So what we will do is have `show`{.haskell} call a helper function `show'`{.haskell}, which would do the heavy lifting and use recursion, and then insert the output between `"{"`{.haskell} and `"}"`{.haskell}. The base case for `show'`{.haskell} should take a `Null`{.haskell} and return the empty string. And for a non-empty set we have:
```{.haskell}
show' Set n left right = show' left ++ "," ++ show n ++ "," ++ show' right
```
 Does this work? Clearly not, for `show' $ Null <<- 1`{.haskell} would be `",1,"`{.haskell}. We need to be smarter. When the node has an empty child, we should not add a comma on that side. That becomes three different cases.
```{.haskell}
show' Null = ""
show' (Set n Null Null) = show n
show' (Set n Null right) = show n ++ "," ++ show' right
show' (Set n left Null) = show' left ++ "," ++ show n
show' (Set n left right) = show' left ++ "," ++ show n ++ "," ++ show' right
```
There is clearly some repetition of logic here. After all, whether it\'s on the left or right, a comma is inserted if and only if the tree is non-empty. That is, `comma Null = ""`{.haskell} and `comma _ = ","`{.haskell}. Using this, we can write a much more compact `show'`{.haskell}. Also, notice that we are calling `show n`{.haskell}, which assumes that `n`{.haskell} is a member of `Show`{.haskell}. This must be added as a constraint in the type signature.
```{.haskell}
instance Show t => Show (Set t) where
    show set = "{" ++ show' set ++ "}" where
        show' Null = ""
        show' (Set n left right) = show' left ++ comma left ++ show n ++ comma right ++ show' right where
            comma Null = ""
            comma _ = ","
```
Let\'s try this out.
```{.haskell}
Prelude> foldl (<<-) Null [1..10]
{1,2,3,4,5,6,7,8,9,10}
```
Beautiful!

The most basic operation (or relation) for sets is membership. And that is easy to define. No element is a member of the empty set. An element is a member of a non-empty set if and only if it is either equal to the root element, or smaller than the root element and a member of the left child, or greater than the root element and a member of the right child (and this is what makes this represention of sets efficient).
```{.haskell}
infix 4 <<=
(<<=) :: t -> Set t -> Bool
_ <<= Null = False
n <<= (Set m left right) = n == m || (n < m && n <<= left) || (n > m && n <<= right)
```

Finally, we are ready to define permutations! We will not restrict permutations to arrangements of $[n]$ (partly because this is difficult to do, and partly because we will certainly have to revise this definition later). A permutation is just a list then, for a list (in Haskell) is naturally ordered from its first element to its last. But it is a list in which elements do not repeat. So once again, we have to define a type for permutations and then write a function that constructs only valid permutations. To define a permutation, we simply wrap a list in a constructor. Rather than using a data declaration, we will use a <a href="https://wiki.haskell.org/Type#Type_and_newtype" title="HaskellWiki – Type and newtype" target="_blank">newtype</a>.
```{.haskell}
newtype Perm t = Perm [t] deriving
```
A newtype combines the efficiency of a `type`{.haskell} (`Perm`{.haskell}s and lists are indistinguishable in memory) with the safety of a data declaration (the wrapper ensures that `Perm`{.haskell}s and lists are not interchange in code). And there are some additional benefits.
```{.haskell}
newtype Perm t = Perm {pList :: [t]} deriving (Eq, Ord)
```
Now `pList`{.haskell} is a function that let\'s us easily extract the underlying list of a permutation whenever pattern matching is inconvenient, and comparing permutations is equivalent to comparing their underlying lists.

As in the case of `Set`{.haskell}, will write a new, smart constructor to replace `Perm`{.haskell}. We do want to construct permutations from lists (anything else would be too tedious), but we will have to discard any repetitions. So we read in the elements of the list one by one, and add it to the permutation only if it is not already present. In fact, we can first write a function to do this (safe) element addition, and use it repeatedly to construct a permutation from a list
```{.haskell}
permIns :: Eq t => Perm t -> t -> Perm t
permIns (Perm ps) n
    | n `elem` ps = Perm ps
    | otherwise = Perm (n:ps)

perm :: Eq t => [t] -> Perm t
perm = Perm . reverse . pList . foldl permIns (Perm [])
```
In `permIns`{.haskell}, we use `elem`{.haskell} for lists (`ps`{.haskell} is the underlying list of the permutation we are constructing), because even though `<<=`{.haskell} for sets is more efficient on the average when we have to repeatedly search an existing set, *constructing* a set by adding elements one at a time is inefficient compared to constructing a list in the same way – adding an element to the head of a list is done in constant time, whereas inserting an element into a set (a binary search tree) can take linear time in the worst case. Again, in the interest of efficiency, `permIns`{.haskell} adds the element at the head of the permutation (list), which makes it necessary to reverse the final permutation in `perm`{.haskell}.

We have defined and constructed permutations, now all that remains is to generate $n$-permutations. There are some clever an relatively efficient algorithms to do this, but we will brute force it for now. The key idea in *generating* all permutations (naïvely) can be obtained by looking at the recurrence relation for *counting* all permutations. Indeed, this is generally true in all of combinatorics.

The total number of $n$-permutations is $n$ times the total number of $(n - 1)$-permutations:  $n! = n(n - 1)!$. The idea this gives is this:

>Suppose we have all the $(n - 1)!$ permutations of a set of $n - 1$ objects. If we now add an <nobr>$n$<sup>th</sup></nobr> element to this set, then in order to get all of their $n!$ permutations, we must somehow generate $n$ permutations from *each* of the $(n - 1)!$ permutations of the smaler set (so that $n \times (n - 1)! = n!$).

But we do already know how we can generate those $n$ $n$-permutations corresponding to each $(n-1)$-permutation (for that is how we know the reccurence relation in the first place!). From each $(n-1)$-permutation, we can get an $n$-permutation by inserting the new element somewhere in the $(n-1)$-permutation. How many choices of places are there for inserting the element? There are $n - 2$ places *between* the $n - 1$ objects, and there are two more places (extreme left and extreme right), so the answer is $n$. And this is precisely how we will generate the permutations too!

Our `perms`{.haskell} function will take a set as input, convert it to a list (guaranteed to have no repeated elements) and pass this to a helper function `perms'`{.haskell} (since it is more convenient to work with lists and avoid having to unwrap permutations all the time). The output of the helper function will be a list of lists, so `perms`{.haskell} will also have to wrap each permutation in `Perm`{.haskell} and finally return a list of permutations.

In defining `perms'`{.haskell}, the helper function, the main difficulty seems to be in inserting the new element in all possible positions in the $(n-1)$-permutation. This can be broken down into two logical steps. First, we *tear apart* a given list (the $(n-1)$-permutation) into two parts in **all possible ways**. Then we join (concatenate) the two parts, but *place the new element in between first*. Concatenation of lists is easily done in Haskell, but although we could use `take`{.haskell} and `drop`{.haskell} to tear apart a list, it is more efficient to do this recursively and avoid performing the same operation twice. So we will write another helper function `lisa`{.haskell} to tear the list apart.
```{.haskell}
perms :: Ord t => Set t -> [Perm t]
perms = map perm . perms' [[]] . setToList where
    perms' ps [] = ps
    perms' ps (n:ns) = perms'
        [first ++ [n] ++ rest | (first, rest) <- foldr (++) [] $ map lisa ps]
        ns
    lisa [] = [([], [])]
    lisa (p:ps) = ([], p:ps) : [(p:first, rest) | (first, rest) <- lisa ps]
```
The argument `ps`{.haskell} is an accumulator (that stores all permutations generated so far). Then `lisa`{.haskell} `map`{.haskell}ped over them tears each one apart in all possible ways (generating a list of list of list-tuples, which has to be folded once to give us back a list of list-tuples). And then we stitch the list-tuples back together but with the new element in between, and this is our new accumulator.

Let\'s write a `show`{.haskell} function for permutations and then test this!
```{.haskell}
instance Show t => Show (Perm t) where
    show = show . pList
```
Well, that was easy. Here we go then…

```{.haskell}
Prelude> perms $ Null <<- 1 <<- 2 <<- 3
[[3,2,1],[2,3,1],[2,1,3],[3,1,2],[1,3,2],[1,2,3]]
*Prelude> length $ perms $ Null <<- 1 <<- 2 <<- 3 <<- 4
24
*Prelude> length $ perms $ Null <<- 1 <<- 2 <<- 3 <<- 4 <<- 5
120
*Prelude> length $ perms $ Null <<- 1 <<- 2 <<- 3 <<- 4 <<- 5 <<- 6
720
*Prelude> length $ perms $ Null <<- 1 <<- 2 <<- 3 <<- 4 <<- 5 <<- 6 <<- 7
5040
*Prelude> length $ perms $ Null <<- 1 <<- 2 <<- 3 <<- 4 <<- 5 <<- 6 <<- 7 <<- 8
40320
```
And that gives us a tiny verification of the <a href="#prop:n-permutations">proposition</a>.

 Here is the entire <a href="https://github.com/VynM/Comutations/blob/master/haskode/src/M0Introduction.hs" title="GitHub – M0Introduction.hs" target="_blank">module</a>, with some more operations on sets and permutations.

 ```{.haskell}
 {-# LANGUAGE GADTs, StandaloneDeriving #-}

module M0Introduction (Set(Null), (<<=), (<<), (<<-), (->>), (<+>), (<**>),
                        setMap, card, setFoldl, setFoldr, setZipWith, setMin, setMax, listToSet, setToList, setToList', 
                        atom, consInts, finNats, nats,
                        perm, pList, permIns, revPerm, addPerms, perms, permSet) where

data Set t where
    Null :: Set t
    Set :: Ord t => {
        root :: t,
        lSet :: Set t,
        rSet :: Set t
    } -> Set t

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
Null <<- n = Set n Null Null
Set m left right <<- n = Set m left' right' where
    (left', right') = case compare m n of
        LT -> (left, right <<- n)
        GT -> (left <<- n, right)
        EQ -> (left, right)

-- Delete from set
infixl 5 ->>
(->>) :: Set t -> t -> Set t
Null ->> n = Null
(Set m left right) ->> n
    | m < n = Set m left (right ->> n)
    | m > n = Set m (left ->> n) right
    | otherwise = left <+> right

-- Set union
infixl 5 <+>
(<+>) :: Set t -> Set t -> Set t
Null <+> set = set
set <+> Null = set
(Set m left1 right1) <+> (Set n left2 right2) = Set m left' right' <+> res where
    (left', right', res) = case compare m n of
        LT -> (left1, right1 <+> right2 <<- n, left2)
        GT -> (left1 <+> left2 <<- n, right1, right2)
        EQ -> (left1 <+> left2, right1 <+> right2, Null)

-- Set transformation by unary function applied to all elements
setMap :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
setMap _ Null = Null
setMap f (Set n left right) = Set (f n) (setMap f left) (setMap f right)

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
atom n = Set n Null Null

-- Set of consecutive integers
consInts :: Integral i => i -> i -> Set i
consInts a b
    | b < a = Null
    | otherwise = Set m (consInts a (m - 1)) (consInts (m + 1) b) where
        m = div (a + b) 2

-- Set of first n naturals
finNats :: Integral i => i -> Set i
finNats = consInts 1

-- Set of all naturals
nats :: Integral i => Set i
nats = Set 1 Null (setMap (+1) nats)

newtype Perm t = Perm {pList :: [t]} deriving (Eq, Ord)

-- Insert into permutation
permIns :: Eq t => Perm t -> t -> Perm t
permIns (Perm ps) n
    | n `elem` ps = Perm ps
    | otherwise = Perm (n:ps)

-- Safe permutation constructor
perm :: Eq t => [t] -> Perm t
perm = Perm . reverse . pList . foldl permIns (Perm [])

instance Show t => Show (Perm t) where
    show = show . pList

-- Reverse of a permutation
revPerm :: Eq t => Perm t -> Perm t
revPerm = Perm . reverse . pList

-- Safe concatenation of permutations
addPerms :: Eq t => Perm t -> Perm t -> Perm t
addPerms (Perm ps) qperm = foldl (permIns) qperm $ reverse ps

-- List of all permutations of a set
perms :: Ord t => Set t -> [Perm t]
perms = map perm . perms' [[]] . setToList where
    perms' ps [] = ps
    perms' ps (n:ns) = perms' [first ++ [n] ++ rest | (first, rest) <- foldr (++) [] $ map lisa ps] ns
    lisa [] = [([], [])]
    lisa (p:ps) = ([], p:ps) : [(p:first, rest) | (first, rest) <- lisa ps]

-- Set of all permutations of a set
permSet :: Ord t => (Set t) -> Set (Perm t)
permSet = listToSet . perms
```
While exporting the module, we hide the unsafe constructors `Set`{.haskell} and `Perm`{.haskell}.