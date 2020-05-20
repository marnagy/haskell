-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Node (Maybe v) [(k, Trie k v)] | Last (Maybe v)
    deriving (Show)

-- Implementujte následující:

empty :: Trie k v
empty = Last Nothing

-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []
--

singleton :: [k] -> v -> Trie k v
singleton [] v = Last (Just v)
singleton (x:xs) v = Node Nothing [(x, singleton xs v )]  

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith _ [] v (Last Nothing) = Last (Just v)
insertWith f [] v (Last (Just v1)) = Last (Just (f v v1))
insertWith _ (x:xs) v (Last val) = Node val [(x, singleton xs v)]
insertWith _ [] v (Node (Nothing) next) = Node (Just v) next
insertWith f [] v (Node (Just v1) next) = Node (Just (f v v1)) next
insertWith f (x:xs) v (Node v1 next) = case (contains x next) of
    False   -> Node v1 ((x, singleton xs v):next)
    True    -> Node v1 (replace f (x:xs) v next)
    where
        replace :: (Ord k) => (v -> v -> v) -> [k] -> v -> [(k, Trie k v)] -> [(k, Trie k v)]
        replace f (k:ks) v ((k1, v1):xs)
            | k == k1   = ((k1, insertWith f ks v v1):xs)
            | otherwise = (k1,v1):replace f (k:ks) v xs
        replace _ [] _ _ = error "Logical error in insertWith code - 1"
        replace _ _ _ [] = error "Logical error in insertWith code - 2"

contains :: Eq a => a -> [(a,b)] -> Bool
contains y ((k,_):xs)
    | y == k    = True
    | otherwise = contains y xs
contains _ [] = False

-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
--

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert = insertWith (\v1 _ -> v1)

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find [] (Last val) = val
find [] (Node val _) = val 
find (_:_) (Last _) = Nothing
find z@(x:xs) (Node _ next) = case (contains x next) of
    True    -> find' z next
    False   -> Nothing
    where
        find' :: (Ord k) => [k] -> [(k, Trie k v)] -> Maybe v
        find' (k:ks) ((k1, t):ns)
            | k == k1   = find ks t
            | otherwise = find' (k:ks) ns
        find' _ _ = Nothing

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member k t = case (find k t) of
    Nothing -> False
    Just _ -> True
--member z t = do
--    val <- find z t
--    hasValue
--    where
--        isValid Nothing = False
--        isValid (Just _) = True

-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'
--
--
-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = undefined

-- BONUS) Implementujte funkci

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete [] (Last _) = Last Nothing
delete [] (Node _ next) = Node Nothing next
delete (x:xs) (Last val) = Last val
delete (x:xs) (Node val next) = case (delete' (x:xs) next) of
    []  -> Last val
    -- not empty case
    arr -> Node val arr
    where
        delete' :: (Ord k) => [k] -> [(k, Trie k v)] -> [(k, Trie k v)]        
        delete' z@(y:ys) ((k, t):ns)
            | y == k    = case (delete ys t) of
                Last Nothing    -> ns
                other     -> (k, other):ns
            | otherwise = (k, t): delete' z ns
        delete' _ _ = []


-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []