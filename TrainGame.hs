

-- thing :: [Int] -> [([Int], [Int])]
-- thing [] = []
-- thing (x : xs) = fstmap (\(as, bs) -> (x:as, bs)) (thing xs)

-- fstmap :: (a -> b) -> [(a, c)] -> [(b, c)]
-- fstmap f ((a,c) : ps) = (f a, c) : fstmap f ps
-- fstmap _ [] = []

trainGame :: [Float] -> Bool
trainGame xs = 10 `elem` (map fst $ allNums xs)

allNums :: [Float] -> [(Float, String)]
allNums [] = []
allNums [x] = [(x, show x)]
allNums (x:xs) | x == 0 = let res = allNums xs in
                          map (showWorking (x *) '*') res
                       ++ map (showWorking (x +) '+') res
                       ++ map (showWorking (x -) '-') res
                 | otherwise = let res = allNums xs in
                               map (showWorking (x *) '*') res
                            ++ map (showWorking (x +) '+') res
                            ++ map (showWorking (x -) '-') res
                            ++ map (showWorking (x /) '/') (filter ((0 /=) . fst) res)
  where
    showWorking :: (Float -> Float) -> Char -> (Float, String) -> (Float, String)
    showWorking op opc (a, ans) = (op  a, show x ++ " " ++ [opc] ++ " " ++ "(" ++ ans ++ ")")

uniq :: Eq a => [a] -> [a]
uniq (x:xs) = let res = uniq xs
              in if x `elem` res then res else x:res
uniq [] = []


data Exp = NumExp
         | OpExp Exp Exp
         deriving (Show, Eq)
          -- | PlusExp Exp Exp
          -- | TimesExp Exp Exp
          -- | DivExp Exp Exp
          -- | MinusExp Exp Exp

trainGame2 :: [Float] -> [(Float, String)]
trainGame2 xs = let tree = unparsing $ length xs in
                uniq $ trainGame2' tree
  where
    trainGame2' :: [Exp] -> [(Float, String)]
    trainGame2' (exp:exps) = (fst $ evaluate2 xs exp) ++ trainGame2' exps
    trainGame2' [] = []

-- unparsing :: Int ->  [Exp]
-- unparsing n | n <= 1 = [NumExp]
--             | n > 1 = map (Parens . OpExp NumExp) (unparsing (n - 1))
--                    ++ map (OpExp NumExp) (unparsing (n - 1))

unparsing :: Int -> [Exp]
unparsing n | n <= 1 = [NumExp]
            | n > 1 = map (OpExp NumExp) (unparsing (n - 1))
                   ++ map (\exp -> OpExp exp NumExp) (unparsing (n - 1))

-- evaluate :: [Float] -> Exp -> ([Float], [Float])
-- evaluate [] _ = error "This is bad"
-- evaluate (x : xs) NumExp = ([x], xs)
-- evaluate xs (OpExp e1 e2) = case evaluate xs e1 of
--                               (res1, rest1) -> case evaluate rest1 e2 of
--                                                (res2, rest2) -> let pairs = perm res1 res2 in
--                                                             (map (joinPair (+)) pairs
--                                                             ++ map (joinPair (-)) pairs
--                                                             ++ map (joinPair (*)) pairs
--                                                             ++ map (joinPair (/)) pairs, rest2)
-- evaluate xs (Parens e) = evaluate xs e

evaluate2 :: [Float] -> Exp -> ([(Float, String)], [Float])
evaluate2 (x : xs) NumExp = ([(x, show x)], xs)
evaluate2 xs (OpExp e1 e2) = case evaluate2 xs e1 of
                               (res1, rest1) -> case evaluate2 rest1 e2 of
                                                  (res2, rest2) -> let pairs = perm res1 res2 in
                                                            (map (joinPair (joinfun (+) '+')) pairs
                                                            ++ map (joinPair (joinfun (-) '-')) pairs
                                                            ++ map (joinPair (joinfun (*) '*')) pairs
                                                            ++ map (joinPair (joinfun (/) '/')) pairs, rest2)
  where
    joinfun :: (Float -> Float -> Float) -> Char -> (Float, String) -> (Float, String) -> (Float, String)
    joinfun op c (r1, eqn1) (r2, eqn2) = (r1 `op` r2, "(" ++ eqn1 ++ [' ', c, ' '] ++ eqn2 ++ ")")

-- fold for tuples
joinPair :: (a -> b -> c) -> (a, b) -> c
joinPair f (a, b) = f a b

-- permutations
perm :: [a] -> [a] -> [(a, a)]
perm (a:as) (b:bs) = (a,b) : (map ((,) a) bs) ++ perm as (b:bs)
perm _ _ = []

