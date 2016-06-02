{- README

Data Structures
-----------------

The *Tree* structure used here is one of

* a Branch, containing a datum which holds all problem-specific logic. This
  should hold all info required to make decisions about the next move. For
  exmaple, in a game of checkers, the node value at a branch might contain
  the state of the board at that node.

* a Leaf, which just contains a Float value corresponding to the hueristic
  weight of exploring that section of the tree


*Zippers* are data structure used to interact with trees and lists. They allow
you to modify inner parts of immutable objects by storing the object in a
special way.

Consider the list `[1,2,3,4,5]`. If you wanted to modify the
3rd element, you could use a zipper consisting of a tuple of three things:
the left part of the list, the current value, and the right part of the list.
Initially this zipper looks just like this: `([], 1, [2,3,4,5])`.
moving the focus to the right twice produces:
`([2,1], 3, [4,5])`.

Note that the "left part" list is reversed. This is so
it is more efficient to go back to the left, ie you just pop the first thing
off of the left part, use that as the new focus, and tack the old focus onto
the beginning of the right part. This idea can be extended to trees as well,
which is exactly what the `Zipper` declaration below is. Check out the
"learn you a haskell" tutorial for a better explanation of zippers,
including trees.


Usage
-------

* Create a datum for your branch nodes, ie the `a` in `Tree a`.

* Create the root Tree with the initial state at the first branch and the
  children leaves containing float heuristic values

* Implement a NodeBuilder function that turns a [zipper focussed on a] leaf
  into a [zipper focussed on a] new branch. The new branch represents one step
  of AStar. The children of this branch should all be leaves, again containing
  a float value corresponding to the heuristic.

That should be all that is necessary, and you can now run the algorithm with
`aStar numOfSteps nodeBuilder rootZipper`

-}


module AI.Tree.AStar ( Tree(Leaf,Branch)
                     , subtrees
                     , leafValue
                     , treeNode
                     , treeScore
                     , isLeaf
                     , Zipper
                     , returnZipper
                     , focus
                     , topOf
                     , ascend
                     , descend
                     , descendPath
                     , directions
                     , replaceFocus
                     , addChild
                     , descendants
                     , leafZippers
                     , sumScores
                     , NodeBuilder
                     , aStar
                     ) where


import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord


{-------.
         `.    HELPERS
           `'------------------------------------------------------------------}


-- | This helper takes a list of maybes, takes everything (left to right) until
-- a Nothing is encountered, and unpacks these values out of the Maybe structure
fromJustWhileJust :: [Maybe a] -> [a]
fromJustWhileJust listOfMaybes = map fromJust $ takeWhile isJust listOfMaybes


{-------.
         `.    TREE HELPERS
           `'------------------------------------------------------------------}


-- | Heuristic tree. Leaves must be Floats (the heuristic) and the Float value
-- in the parental nodes represent the score of that node. The Float in parental
-- nodes is the score of that path from it's parent.
data Tree a = Leaf Float | Branch a Float [Tree a] --deriving (Show)


instance Show a => Show (Tree a) where
  show tree =
    intercalate "\n" $ [tree] >>= showTree


showTree :: Show a => Tree a -> [String]
showTree tree =
  if isLeaf tree
    then [ "[ " ++ show (leafValue tree) ++ " ]" ]
    else let
          Branch x f ts = tree
          tup = "(" ++ show x ++ ", " ++ show f ++ ") "
          blank = replicate (length tup) ' '
          left = tup : repeat blank
          right = ts >>= showTree
         in zipWith (++) left right


subtrees :: Tree a -> [Tree a]
subtrees (Branch _ _ ts) = ts


leafValue :: Tree a -> Float
leafValue (Leaf x) = x


treeNode :: Tree a -> a
treeNode (Branch x _ _) = x


treeScore :: Tree a -> Float
treeScore (Branch _ x _) = x


isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False


{-------.
         `.    ZIPPER HELPERS
           `'------------------------------------------------------------------}


-- | Zipper to keep track of Trees.
-- Structure of the tuple is:
--   ( current focus tree
--   , inverted list of parent node values (trees stripped of their children)
--   , inverted list of parent score values (Floats)
--   , inverted list of (list of left nodes of parent tree)
--   , inverted list of (list of right nodes of parent tree)
--   )
type Zipper a = (Tree a, [a], [Float], [[Tree a]], [[Tree a]])


returnZipper :: Tree a -> Zipper a
returnZipper tree = (tree, [], [], [], [])


focus :: Zipper a -> Tree a
focus (t, _, _, _, _) = t


-- | Move focus of zipper to the top of the tree
topOf :: Zipper a -> Zipper a
topOf zpr =
  case ascend zpr of
    Nothing -> zpr
    Just z -> topOf z


-- | Go up one level of the zipper, return Nothing if at the top already
ascend :: Zipper a -> Maybe (Zipper a)
ascend (_, [], _, _, _) = Nothing
ascend (t, p:ps, s:ss, l:ls, r:rs) =
  Just (Branch p s (l ++ t:r), ps, ss, ls, rs)


-- | Descend into the ith subtree of the zipper
descend :: Int -> Zipper a -> Maybe (Zipper a)
descend _ (Leaf _, _, _, _, _) = Nothing
descend i (Branch x s ts, parents, scores, lefts, rights) =
  if i >= length ts
    then Nothing
    else let (start, t:end) = splitAt i ts
         in Just (t, x:parents, s:scores, start:lefts, end:rights)


-- | Repeatedly descend until the directions list is exhausted. Return Nothing
-- if we fell off the tree, otherwise the zipper wrapped in Just
descendPath :: [Int] -> Zipper a -> Maybe (Zipper a)
descendPath [] zpr = Just zpr
descendPath (p:path) zpr =
  case descend p zpr of
    Nothing -> Nothing
    Just z -> descendPath path z


-- | get directions to the currently focussed subtree
directions :: Zipper a -> [Int]
directions (_, _, _, lefts, _) = reverse $ map length lefts


replaceFocus :: Zipper a -> Tree a -> Zipper a
replaceFocus (_, a, b, c, d) tree = (tree, a, b, c, d)


-- | Adds node at the ith posn of subtree of the currently focussed tree
-- (repackage the zipper)
addChild :: Zipper a -> Int -> Tree a -> Zipper a
addChild (Branch x s ts, ss, ps, ls, rs) i node =
  let (start, end) = splitAt i ts
  in (Branch x s (start ++ node:end), ss, ps, ls, rs)


-- | Map the subtrees of the focus of the input zipper into zippers with the
-- focus moved to each respective subtree. Return Nothing if there are no
-- children, and wrap in Just otherwise
descendants :: Zipper a -> Maybe [Zipper a]
descendants zpr =
  let f = focus zpr
  in if isLeaf f || null (subtrees f)
      then Nothing
      else Just $ fromJustWhileJust $ map (`descend` zpr) [0..]


-- | map each leaf in tree to it's corresponding zipper
leafZippers :: Zipper a -> [Zipper a]
leafZippers zpr =
  case descendants zpr of
    Nothing -> [zpr]
    Just ds -> ds >>= leafZippers


-- | Repeatedly ascend until top is reached, adding up the score of each node
sumScores :: Zipper a -> Float
sumScores zpr =
  let
    f = focus zpr
    s = if isLeaf f then 0 else treeScore f
  in case ascend zpr of
      Nothing -> s
      Just z -> s + sumScores z


{-------.
         `.    A*
           `'------------------------------------------------------------------}


-- | A node builder is a function that turns a leaf into a tree
--    * consumes zipper focussed on the leaf to replace
--    * heuristic is used within the node builder when adding the leaves
--    * produce a new zipper with the leaf replaced
type NodeBuilder a = Zipper a -> Zipper a


-- | aStarStep uses A* to add the next most promising step to the tree.
-- The input tree is such that all leaves are sparse nodes containing only
-- the heuristic value and pieces.
--
-- Plan:
--  * find largest heuristic among leaves
--  * add node to the winning heuristic
aStarStep :: NodeBuilder a -> Zipper a -> Zipper a
aStarStep buildNode zpr =
  topOf.buildNode $ maximumBy (comparing $ leafValue.focus) (leafZippers zpr)


-- | sum up rough scores for each path in the tree and return the one most
-- likely to win. Return Nothing if the tree is just a single leaf
descendBestPath :: Zipper a -> Maybe (Zipper a)
descendBestPath zpr =
  ascend $ maximumBy (comparing sumScores) (leafZippers zpr)


-- | Perform A* step on the given zipper n times, using buildNode to transform
-- the best leaf into a new subtree, then return a zipper focussed on the
-- leaf node corresponding to the best path. Returns Nothing if the tree
-- is just a leaf.
aStar :: Int -> NodeBuilder a -> Zipper a -> Maybe (Zipper a)
aStar n buildNode zpr =
  descendBestPath . last $ take (n+1) $ iterate (aStarStep buildNode) zpr


{-------.
         `.    TEST
           `'------------------------------------------------------------------}


testTree :: Tree Int
testTree =
  Branch 9 1.1 [ Branch 9 32.1 [ Leaf 0.5, Leaf 0.6 ]
              , Leaf 0.4
              , Branch 9 2.2 [ Leaf 0.9
                            , Branch 8 99.2 [ Leaf 0.9 ]
                            ]
              ]


nb :: NodeBuilder Int
nb zpr =
  replaceFocus zpr (Branch 7 6.6 [Leaf 0.1, Leaf 0.2])


test :: IO ()
test = do
  let tree = testTree
  putStrLn "\n[ PRINT BOMB ]\n"
  print tree
  putStrLn "\n"
  -- print $ focus.fromJust $ descend (returnZipper tree) 0
  -- putStrLn "\n"
  -- print $ focus.fromJust.ascend.fromJust $ descend (returnZipper tree) 0
  -- putStrLn "\n"
  -- mapM_ (print.focus) $ fromJust $ descendants (returnZipper tree)
  -- putStrLn "\n"
  -- mapM_ (print.focus) $ leafZippers (returnZipper tree)
  -- putStrLn "\n"
  -- print $ sumScores $ fromJust $ descend (returnZipper tree) 0
  -- putStrLn "\n"
  -- print $ focus $ topOf $ aStarStep nb (returnZipper tree)
  -- putStrLn "\n"
  -- print $ directions . fromJust $ descendBestPath (returnZipper tree)
  -- putStrLn "\n"
  print $ focus . topOf . fromJust $ aStar 2 nb (returnZipper tree)
  putStrLn "\n"
  -- putStrLn "\n\n[ TEST ]\n"


{-------.
         `.    MAIN
           `'------------------------------------------------------------------}


main :: IO ()
main =
  test
