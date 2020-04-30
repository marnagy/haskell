-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Node (Maybe v) [(k, Trie k v)] | Last
    deriving (Show)

-- Implementujte následující:

empty :: Trie k v
empty = Last

-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []
--

singleton :: [k] -> v -> Trie k v
singleton [] v = Node (Just v) []
singleton (x:xs) v = Node Nothing [(x, singleton xs v )]  

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith _ [] v Last = Node (Just v) []
insertWith _ (x:xs) v Last = Node Nothing [(x, singleton xs v)]
insertWith _ [] v (Node (Nothing) next) = Node (Just v) next
insertWith f [] v (Node (Just v1) next) = Node (Just (f v v1)) next
insertWith f (x:xs) v (Node v1 next) = case (get x next) of
    False   -> Node v1 ((x, singleton xs v):next)
    True    -> Node v1 (replace f (x:xs) v next)
    where
        replace :: (Ord k) => (v -> v -> v) -> [k] -> v -> [(k, Trie k v)] -> [(k, Trie k v)]
        replace f (k:ks) v ((k1, v1):xs)
            | k == k1   = ((k1, insertWith f ks v v1):xs)
            | otherwise = (k1,v1):replace f (k:ks) v xs
        replace _ [] _ _ = error "Logical error in insertWith code - 1"
        replace _ _ _ [] = error "Logical error in insertWith code - 2"

get :: Eq a => a -> [(a,b)] -> Bool
get y ((k,_):xs)
    | y == k    = True
    | otherwise = get y xs
get _ [] = False

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
find = undefined

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
delete = undefined

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []