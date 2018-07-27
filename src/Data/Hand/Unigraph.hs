module Data.Hand.Unigraph where

data Unigraph a = Node a [Unigraph a] deriving (Show)

instance (Eq a) => Eq (Unigraph a) where
    Node hx tx == Node hy ty = (hx == hy) && (tx == ty)

comboGraph :: [a] -> Int -> [Unigraph a]
comboGraph _ 0 = []
comboGraph [] _ = []
comboGraph (x:xs) n =
    buildEdge x xs : (comboGraph xs n)
    where   buildEdge h t = Node h (comboGraph t (n-1))

paths :: Unigraph a -> [[a]]
paths (Node a []) = [[a]]
paths (Node a (x:xs)) =
    (depths x) ++ (breadths xs)
    where   depths node = (map (a:) (paths node))
            breadths nodes = concatMap depths nodes

combos :: [a] -> Int -> [[a]]
combos items n = filterDepth (concatMap paths (comboGraph items n))
    where filterDepth xs = filter (\x -> (length x == n)) xs