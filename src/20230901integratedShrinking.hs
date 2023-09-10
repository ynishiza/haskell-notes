#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Function
import Data.Functor
import Data.List (intercalate)
import Data.Maybe
import GHC.Exts (IsString)
import System.Random
import Test.Hspec

{-
  Based on: https://www.well-typed.com/blog/2019/05/integrated-shrinking/
-}
main :: IO ()
main = do
  printIntegratedGen iBool
  printIntegratedGen @Int iSmallIntegral

  putStrLn $ banner "Applicative vs Monad"
  printIntegratedGen $ (,) <$> pureIntegrated tBool <*> pureIntegrated tInt1
  printIntegratedGen $ (,) <$> pureIntegrated tInt1 <*> pureIntegrated tBool
  printIntegratedGen $ pureIntegrated tInt1 >>= \x -> (x,) <$> pureIntegrated tBool

  printIntegratedGen $ pureIntegrated tInt1 >>= \x -> (x,) <$> pureIntegrated tBool
  putStrLn $ banner "Test"
  runTest

printIntegratedGen :: Show a => IntegratedGen a -> IO ()
printIntegratedGen g = runIntegratedGen g >>= putStrLn . prettyPrintTree

banner :: (IsString s, Semigroup s) => s -> s
banner name = "=====" <> name <> "====="

pureIntegrated :: Tree a -> IntegratedGen a
pureIntegrated t = IntegratedGen $ const t

tBool :: Tree Bool
tBool = Node True [Node False []]

tInt1 :: Tree Int
tInt1 = Node 1 [Node 0 []]

tInt2 :: Tree Int
tInt2 = Node 2 [Node 1 [Node 0 []]]

tInt3 :: Tree Int
tInt3 = Node 2 [Node 1 [Node 0 []], Node 0 []]

iBool :: IntegratedGen Bool
iBool = integratedGenFromUniform shrinker
  where
    shrinker True = [False]
    shrinker False = []

iSmallIntegral :: (Uniform i, Integral i) => IntegratedGen i
iSmallIntegral = IntegratedGen $ \rnd -> growTree shrinker ((`mod` 9) <$> fst $ uniform rnd)
  where
    shrinker x =
      [ if x > 0 then Just (x - 1) else Nothing,
        if x > 2 then Just (x `div` 2) else Nothing
      ]
        & catMaybes

-- ==============================  Test ==============================
-- ================================================================================

runTest :: IO ()
runTest = hspec test

test :: Spec
test = describe "Integrated Shrinker" $ do
  it "[pickOne]" $ do
    pickOne [1 :: Int, 2, 3] `shouldBe` [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]

  it "[Applicative] combine generators with applicative" $ do
    tree1 <- runIntegratedGen $ (,) <$> pureIntegrated tBool <*> pureIntegrated tInt1
    tree1
      `shouldBe` Node
        (True, 1)
        [ Node (False, 1) [Node (False, 0) []],
          Node (True, 0) [Node (False, 0) []]
        ]

    tree2 <- runIntegratedGen $ (,) <$> pureIntegrated tBool <*> pureIntegrated tInt2
    tree2
      `shouldBe` Node
        (True, 2)
        [ Node
            (False, 2)
            [ Node (False, 1) [Node (False, 0) []]
            ],
          Node
            (True, 1)
            [ Node (False, 1) [Node (False, 0) []],
              Node (True, 0) [Node (False, 0) []]
            ]
        ]

  it "[Applicative] does not generate a good tree with monad" $ do
    goodTree <- runIntegratedGen $ (,) <$> pureIntegrated tBool <*> pureIntegrated tInt2
    goodTree
      `shouldBe` Node
        (True, 2)
        [ Node
            (False, 2)
            [ Node (False, 1) [Node (False, 0) []]
            ],
          Node
            (True, 1)
            [ Node (False, 1) [Node (False, 0) []],
              Node (True, 0) [Node (False, 0) []]
            ]
        ]

    badTree <- runIntegratedGen $ pureIntegrated tBool >>= \x -> (x,) <$> pureIntegrated tInt2
    badTree
      `shouldBe` Node
        (True, 2)
        [ Node
            (False, 2)
            [ Node (False, 1) [Node (False, 0) []]
            ],
          Node
            (True, 1)
            [ Node (True, 0) []
            ]
        ]

  it "[iListOfSize] shrinking list of fixed length" $ do
    listTree <- runIntegratedGen $ iListOfSize 3 (pureIntegrated tBool)
    listTree
      `shouldBe` Node
        [True, True, True]
        [ Node
            [False, True, True]
            [ Node [False, False, True] [Node [False, False, False] []],
              Node [False, True, False] [Node [False, False, False] []]
            ],
          Node
            [True, False, True]
            [ Node [False, False, True] [Node [False, False, False] []],
              Node [True, False, False] [Node [False, False, False] []]
            ],
          Node
            [True, True, False]
            [ Node [False, True, False] [Node [False, False, False] []],
              Node [True, False, False] [Node [False, False, False] []]
            ]
        ]

  it "[iList] shrinking list of random length" $ do
    listTree <- runIntegratedGen $ iList (pure 3) (pureIntegrated tBool)
    treeSize listTree `shouldBe` 550
    root listTree `shouldBe` [True, True, True]
    root <$> subtrees listTree
      `shouldBe` [
                   -- First value omitted
                   [True, True],
                   -- First value changed
                   [False, True, True],
                   -- Second value omitted
                   [True, True],
                   -- Second value changed
                   [True, False, True],
                   -- Third value omitted
                   [True, True],
                   -- Third value changed
                   [True, True, False]
                 ]

    -- subtrees of [True, True]
    root <$> subtrees (head (subtrees listTree))
      `shouldBe` [
                   -- First value omitted
                   [True],
                   -- First value changed
                   [False, True],
                   -- Second value omitted
                   [True],
                   -- Second value changed
                   [True, False]
                 ]

    runIntegratedGen (iList (pure 3) (pureIntegrated tInt2))
      >>= (`shouldBe` 12268) . treeSize
    runIntegratedGen (iList (pure 3) (pureIntegrated tInt3))
      >>= (`shouldBe` 24136) . treeSize

-- ==============================  Tree ==============================
-- ================================================================================

data Tree a = Node {root :: a, subtrees :: [Tree a]}
  deriving stock (Eq, Functor)

instance Show a => Show (Tree a) where show = prettyPrintTree

treeSize :: Tree a -> Int
treeSize Node {..} = 1 + sum (treeSize <$> subtrees)

prettyPrintTree :: Show a => Tree a -> String
prettyPrintTree = (<> "\n") . intercalate "\n" . print_
  where
    print_ :: Show a => Tree a -> [String]
    print_ Node {..} = ("" <> show root) : rest
      where
        rest =
          subtrees
            <&> zip [0 :: Int ..] . print_
            & zip [0 :: Int ..]
            <&> (\(i, x) -> getPrefix i <$> x)
            & join
        l = length subtrees
        getPrefix _ (0, s) = "|- " <> s
        getPrefix i (_, s) = (if i < (l - 1) then "|  " else "   ") <> s

type TreeStep a = a -> [a]

growTree :: forall a. TreeStep a -> a -> Tree a
growTree shrinkStep rootValue = Node rootValue (growTree shrinkStep <$> shrinkStep rootValue)

-- ==============================  Generator ==============================
-- ================================================================================

newtype Gen a = Gen (StdGen -> a)
  deriving stock (Functor)
  deriving newtype (Applicative, Monad)

newtype IntegratedGen a = IntegratedGen (StdGen -> Tree a)
  deriving stock (Functor)

splitN :: Int -> StdGen -> [StdGen]
splitN n rnd
  | n <= 0 = []
  | otherwise = let (rnd1, rnd2) = split rnd in rnd1 : splitN (n - 1) rnd2

runIntegratedGen :: IntegratedGen a -> IO (Tree a)
runIntegratedGen (IntegratedGen f) = getStdRandom (\rnd -> let (rnd1, rnd2) = split rnd in (f rnd1, rnd2))

runGen :: Gen a -> StdGen -> a
runGen (Gen gen) = gen

integratedGenFromUniform :: forall a. Uniform a => TreeStep a -> IntegratedGen a
integratedGenFromUniform shrinkStep = IntegratedGen $
  \rnd -> let (newRoot, _) = uniform rnd in growTree shrinkStep newRoot

instance Applicative IntegratedGen where
  pure a = IntegratedGen $ \_ -> Node a []
  IntegratedGen fgen <*> IntegratedGen agen = IntegratedGen $ \rnd ->
    let (rnd1, rnd2) = split rnd
     in applyTrees (fgen rnd1) (agen rnd2)
    where
      applyTrees :: Tree (a -> b) -> Tree a -> Tree b
      applyTrees ftree@(Node froot fsubtrees) atree@(Node {..}) =
        Node
          (froot root)
          $ (flip applyTrees atree <$> fsubtrees) ++ (applyTrees ftree <$> subtrees)

-- Generator for list of fixed size
iListOfSize :: Int -> IntegratedGen a -> IntegratedGen [a]
iListOfSize 0 _ = pure []
iListOfSize n x = (:) <$> x <*> iListOfSize (n - 1) x

-- Generator for list with random size
iList :: IntegratedGen Int -> IntegratedGen a -> IntegratedGen [a]
iList (IntegratedGen ngen) (IntegratedGen agen) = IntegratedGen $ \rnd ->
  let (rnd1, rnd2) = split rnd
      -- length of list
      (Node n _) = ngen rnd1
      -- elements of list
      listElements =
        replicateM n (Gen agen)
          & flip runGen rnd2
   in listWithValues listElements
  where
    listWithValues :: [Tree a] -> Tree [a]
    listWithValues trees = Node (root <$> trees) $ do
      (tinit, Node _ subtrees, ttail) <- pickOne trees
      -- case: omit one term
      listWithValues (tinit ++ ttail)
        :
        -- case: replace term
        ((\a -> listWithValues (tinit ++ a : ttail)) <$> subtrees)

pickOne :: [a] -> [([a], a, [a])]
pickOne [] = []
pickOne (a : as) = ([], a, as) : (prependA <$> pickOne as)
  where
    prependA (ainit, a', atail) = (a : ainit, a', atail)

-- ==============================  BAD monad ==============================
-- ================================================================================

joinTree :: Tree (Tree a) -> Tree a
joinTree (Node (Node {..}) xs) = Node root $ (joinTree <$> xs) ++ subtrees

generateInnerTrees :: StdGen -> Tree (IntegratedGen a) -> Tree (Tree a)
generateInnerTrees rnd (Node (IntegratedGen gen) rest) = Node tree subtrees
  where
    tree = gen rnd
    subtrees =
      zip (splitN (length rest) rnd) rest
        <&> uncurry generateInnerTrees

instance Monad IntegratedGen where
  IntegratedGen gen >>= k = IntegratedGen $ \rnd ->
    let (rnd1, rnd2) = split rnd
        t = k <$> gen rnd1
     in joinTree $ generateInnerTrees rnd2 t
