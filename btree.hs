{-The Nil constructor is an empty list. It contains no objects. 
So any time you're using the [] expression, you're actually using Nil.-}

-- emptyList :: [Int]
-- emptyList = [] -- Actually Nil

-- fullList :: [Int]
-- -- Equivalent to Cons 1 (Cons 2 (Cons 3 Nil))
-- -- More commonly written as [1,2,3]
-- fullList = 1 : 2 : 3 : []

data Tree a =  Nil
            |   Leaf Int [a] 
            |   Node Int [a] [Tree a]
            deriving(Show)

-- Crear desde un [a].
{- Properties of B-Tree: 
If m is the order of the tree, each internal node can contain at most m - 1 keys along with a pointer to each child.-}
-- Max. Degree  
m = 3

listToTree :: (Ord a, Eq a) => [a] -> Tree a -> Tree a
listToTree [] b = b
listToTree (x:xs) b = listToTree xs (insert b x)

insert :: (Ord a, Eq a) => Tree a -> a -> Tree a
insert t x = if is_full t then insert_non_full (split t) x
                          else insert_non_full t x

insert_non_full :: (Ord a, Eq a) => Tree a -> a -> Tree a
insert_non_full (Nil) x = Leaf m (x:[])
insert_non_full (Leaf m []) x = Leaf m (x:[])
insert_non_full l@(Leaf m keys@(k:ks)) x
  | x == k = l
  | x < k  = Leaf m (x:keys)
  | x > k  = Leaf m (k:new_ks)
    where Leaf _ new_ks = insert_non_full (Leaf m ks) x
insert_non_full (Node m [] (t:ts)) x = if is_full t then insert_non_full (split t) x
                                                    else Node m [] [(insert_non_full t x)]
insert_non_full n@(Node m keys@(k:ks) trees@(t:ts)) x
  | x == k = n
  | x < k  = if is_full t then insert_non_full (Node m (newK:k:ks) (newT1:newT2:ts)) x
                          else Node m keys ((insert_non_full t x):ts)
  | x > k  = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insert_non_full (Node m ks ts) x
          Node _ [newK] [newT1, newT2] = split t

split :: (Ord a, Eq a) => Tree a -> Tree a
split (Leaf m keys) = Node m [k] [Leaf m k1, Leaf m k2]
  where k1 = first_half keys
        k:k2 = last_half keys
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m k2 t2]
  where k1 = first_half keys
        k:k2 = last_half keys
        t1 = first_half trees
        t2 = last_half trees

first_half :: [a] -> [a]
first_half xs = take (div (length xs) 2) xs

last_half :: [a] -> [a]
last_half xs = drop (div (length xs) 2) xs

is_full :: (Ord a, Eq a) => Tree a -> Bool
is_full (Nil) = False
is_full (Leaf m ks)
--  | length ks == (2 * m - 1) = True
    | length ks == (m - 1) = True
    | otherwise = False
is_full (Node m ks _)
--  | length ks == (2 * m - 1) = True
    | length ks == (m - 1) = True
    | otherwise = False

-- Buscar el máximo.
get_max :: (Ord a, Eq a) => Tree a -> a
get_max (Leaf _ keys) = last keys
get_max (Node _ _ trees) = get_max (last trees)

-- Buscar el mínimo.
get_min :: (Ord a, Eq a) => Tree a -> a
get_min (Leaf _ keys) = head keys
get_min (Node _ _ trees) = get_min (head trees)

-- Devolver un array de [a].
get_array :: (Ord a, Eq a) => [a] -> Tree a -> [a]
get_array xs Nil = xs
get_array xs (Leaf m [])  = []
get_array xs (Leaf m keys) = merge keys xs
get_array xs (Node m [] trees) = merge (get_array xs (head trees)) (get_array xs (last trees))
get_array xs (Node m keys trees) = merge keys (get_array xs b)
  where b = Node m [] trees

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Mostrar la cantidad de item en el arbol.
{- Es mostrar el tamaño del arreglo del metodo get_array.-}
get_cant :: (Ord a, Eq a) => Tree a -> Int
get_cant b = mylen (get_array [] b)

mylen :: [a] -> Int
mylen [] = 0
mylen (x:xs) = 1 + mylen xs

-- Mostrar la profundida.
{- La profundidad o altura de un árbol es el máximo nivel de cualquier hoja en el árbol.-}

get_prof :: (Ord a, Eq a) => Tree a -> Int
get_prof Nil =  0
get_prof (Leaf _ _) =  0
get_prof (Node _ _ trees) = 1 + get_prof (head trees) + get_prof (last trees)

{- Print -}
inorder :: (Ord a) => Tree a -> [a]
inorder   Nil = []
inorder   (Leaf _ keys) = keys
inorder   (Node _ keys trees) = inorder (head trees) ++ keys ++ inorder (last trees)

preorder :: (Ord a) => Tree a -> [a]
preorder  Nil = []
preorder  (Leaf _ keys) = keys
preorder  (Node _ keys trees) = keys ++ preorder (head trees) ++ preorder (last trees)

postorder :: (Ord a) => Tree a -> [a]
postorder Nil = []
postorder (Leaf _ keys) = keys
postorder (Node _ keys trees) = postorder (head trees) ++ postorder (last trees) ++ keys
