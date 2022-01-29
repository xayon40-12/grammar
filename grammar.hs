#!/usr/bin/env runghc
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Map (Map,(!),insert,toList,fromList,empty)
import Data.List (isPrefixOf)
import Data.Char

data Rec = Ter | NTer deriving (Show)
data Grammar t = Atom Rec t
                  | Err String
                  | (Grammar t) :. (Grammar t)
                  | (Grammar t) :+ (Grammar t)
                  | Star (Grammar t)
                  | Un (Grammar t)
                  | (Grammar t) :# Int
                  | GTer
                  | GNTer
                  deriving (Show)
infixl 5 :+
infixl 6 :.
infixl 7 :#


t,n :: t -> Grammar t
t t = Atom Ter t
n t = Atom NTer t

s :: Grammar t -> Grammar t
s a = Star a

u :: Grammar t -> Grammar t
u a = Un a

err :: String -> Grammar t
err = Err

box :: String -> (Int,Int,Int,[String])
box s = (ws,1,div ws 2,[s]) where ws = length s
br :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
br s (wl,hl,al,l) (wr,hr,ar,r) = 
  (wl+wr+ws+2,max hl hr,wl+as+1,t0 : t1 : zipWith (\l r-> l++m++r) (l++le) (r++re))
  where m = replicate (ws+2) ' '
        ws = length s
        as = div ws 2
        rp i c = replicate i c
        t0 = rp al ' ' ++ rp (wl-al) '-' ++ " " ++ s ++ " " ++ rp (ar+1) '-' ++ rp (wr-ar-1) ' '
        t1 = rp al ' ' ++ "|" ++ rp (wl-al) ' ' ++ rp (length s) ' ' ++ rp (ar+1) ' ' ++ "|" ++ rp (wr-ar-1) ' '
        e l r = replicate (max 0 (length r - length l)) (replicate (length (head l)) ' ')
        le = e l r
        re = e r l
top :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
top s (wb,hb,ab,b) = (wb,hb+1,ab,sp0 s : spb (sp1 "|" : b)) 
  where sp0 s = replicate (max 0 (ab-ws2)) ' ' ++ s ++ replicate (max 0 (wb-ab-1-ws2)) ' '
        sp1 s = replicate ab ' ' ++ s ++ replicate (wb-ab-1) ' '
        spb ss = (\s->replicate (max 0 (ws2-ab-wsm)) ' ' ++ s ++ replicate (max 0 (ws2-(wb-ab-1))) ' ')<$>ss
        ws = length s
        ws2 = div ws 2
        wsm = 1-mod ws 2

class PShow t where
  pshow :: t -> String

instance PShow Char where pshow = (:[])
instance (PShow a) => PShow [a] where 
  pshow [] = ""
  pshow (a:as) = pshow a <> pshow as

gshow :: (PShow t) => (t,Grammar t) -> String
gshow (name,g) = unlines . (\(_,_,_,i)->i) . top (pshow name) . go id $ g
  where
    go f (Atom Ter t) = box $ f $ "'" ++ pshow t ++ "'"
    go f (Atom NTer t) = box $ f $ pshow t
    go f GTer = box $ f $ "ELTER"
    go f GNTer = box $ f $ "IDNTER"
    go f (a :. b) = br (f ".") (go id a) (go id b)
    go f (a :+ b) = br (f "+") (go id a) (go id b)
    go f (Star a) = go (f.("["++).(++"]")) a
    go f (Un a) = go (f.("(|"++).(++"|)")) a
    go f (Err e) = box $ f $ "Error: " ++ show e
    go f (a :# i) = go (f.(++("#"++show i))) a

pprint = putStrLn . gshow

class Extract t where
  extract :: t -> t -> Either String (Maybe t) -- search second argument in first
  extractTer :: t -> (Maybe t,Either String t)
  extractNTer :: t -> (Maybe t,Either String t)

type Env t = Map t (Grammar t)
type ParseSEnv t = (Maybe t,SEnv t,[Grammar t])
parse :: (Ord t, Extract t, Show t) => Env t -> Env t -> t -> t -> Either String (Env t)
parse m r start t = case parse' m (r,Nothing) (m ! start) t [] of
  (_,_,[Err e]) -> Left e
  (Nothing,(r1,_),_) -> Right r1
  (Just t1,_,_) -> Left $ "Parse did not reach end of input, remaining:\n\t" <> show t1

type SEnv t = (Env t, Maybe t)
parse' :: (Ord t, Extract t) => Env t -> SEnv t -> Grammar t -> t -> [Grammar t] -> ParseSEnv t
parse' m r (Atom Ter a) t g = case extract t a of
  Left e -> (Just t,r,[Err e])
  Right t1 -> (t1,r,g)
parse' m r (Atom NTer a) t g = case parse' m r (m ! a) t [] of
  (_,_,[Err e]) -> (Just t, r, [Err e])
  (t1,r1,g1) -> (t1,r1,g<>g1)
parse' m r GTer t g = withEnv r g Ter $ extractTer t
parse' m r GNTer t g = withEnv r g NTer $ extractNTer t
parse' m r (a :. b) t g = case parse' m r a t g of
  (_,_,[Err e1]) -> (Just t,r,[Err $ "In a Conc left: " <> e1])
  (Just t1,r1,g1) -> case parse' m r1 b t1 g1 of
    (_,_,[Err e2]) -> (Just t1,r1,[Err $ "In a Conc right: " <> e2])
    x -> x
  (Nothing,r1,g1) -> (Nothing,r1,[Err "Reached end of input in a Conc left."])
parse' m r (a :+ b) t g = case parse' m r a t g of
  (_,_,[Err _]) -> case parse' m r b t g of
    (_,_,[Err e2]) -> (Just t,r,[Err $ "In a Conc right: " <> e2])
    x -> x
  x -> x
parse' m r (Star a) t g = case parse' m r a t g of
  (_,_,[Err _]) -> (Just t,r,g) -- It might be necessary to store the hystory here
  (Just t1,r1,g1) -> parse' m r1 (Star a) t1 g1
  (Nothing,r1,_) -> (Just t,r,[Err "Reached end of input in a Star."])
parse' m r (Un a) t g = case parse' m r a t g of
  (_,_,[Err _]) -> (Just t,r,g) -- It might be necessary to store the hystory here
  x -> x
parse' m r (Err e) t _ = (Just t,r,[Err e])
parse' m r (a :# i) t g = parseRule i $ parse' m r a t g

withEnv r g _ (t,Left e) = (t,r,[Err e])
withEnv r g rec (t,Right t1) = (t,r,g<>[Atom rec t1])

parseRule :: (Ord t) => Int -> ParseSEnv t -> ParseSEnv t
parseRule 1 (t,(r,Just k),[g1]) = (t,(insert k g1 r,Nothing),[])
parseRule 1 (t,r,gs) = (t,r,[Err $ "Rule 1 needs exacly one elements stored (found " <> (show (length gs)) <> ") and that the rule 2 already stored a name."])
parseRule 2 (t,(r,_),[Atom _ k]) = (t,(r,Just k),[])
parseRule 2 (t,r,gs) = (t,r,[Err $ "Rule 2 needs exactly one Atom stored, found " <> (show (length gs)) <> "."])
parseRule 3 g = app2 g 3 (:.)
parseRule 4 g = app2 g 4 (:+)
parseRule 5 g = g
parseRule 6 (t,r,[g1]) = (t,r,[Star g1]) 
parseRule 6 (t,r,gs) = (t,r,[Err $ "Rule 6 needs exactly one element parsed, found " <> show (length gs) <> "."]) 
parseRule 7 (t,r,[g1]) = (t,r,[Un g1]) 
parseRule 7 (t,r,gs) = (t,r,[Err $ "Rule 7 needs exactly one element parsed, found " <> show (length gs) <> "."]) 

app2 (t,r,[g1,g2]) _ op = (t,r,[op g1 g2])
app2 (t,r,gs) i _ = (t,r,[Err $ "Rule " <> show i <> " needs exactly two elements already parsed, found " <> show (length gs) <> "."])

-- Extract for String

instance Extract String where
  extract c v | isPrefixOf v c = Right . toMaybe $ drop (length v) c
              | otherwise = Left $ "In extract: Expected " <> show v <> ", found " <> show (take (length v) c) <> "."
  extractTer [] = (Nothing,Left "Empty list when extracting a terminal.")
  extractTer ('\'':cs) = case span (/='\'') cs of
    (e:es,'\'':s) -> (toMaybe s, Right (e:es))
  extractTer (c:cs) = (Just (c:cs), Left $ "Expected a \"'\" to extract a terminal, found \"" <> [c] <> "\".")
  extractNTer [] = (Nothing,Left "Empty list when extracting a non terminal.")
  extractNTer (c:cs) | isAlpha c = case span ((||) <$> isAlpha <*> isDigit) cs of
                                    (es,s) -> (toMaybe s, Right (c:es))
                     | otherwise = (toMaybe cs,Left $ "Expected a capital letter to start a non terminal, found \"" <> [c] <> "\".")

toMaybe [] = Nothing
toMaybe l = Just l
-- main

main = do
  let g0 = fromList [("S",s (n "N" :. t "->" :. n "E" :. t "," :# 1) :. t ";")
                    ,("N",GNTer :# 2)
                    ,("R",GTer :. t "." :. GNTer :# 3)
                    ,("E",n "T" :. s (t "." :. n "T" :# 3))
                    ,("T",n "F" :. s (t "+" :. n "F" :# 4))
                    ,("F",GNTer :# 5 :+ GTer :# 5 :+ t "(" :. n "E" :. t ")" :+ t "[" :. n "E" :. t "]" :# 6 :+ t "(|" :. n "E" :. t "|)" :# 7)
                    ]

  putStrLn "G0:\n--------------------------------------------\n"
  forM_ (toList g0) pprint

  let s = "b->'b'.a.'b',a->'a'.(|b|).'a',;"
  putStrLn $ "Parsing \"" <> s <> "\":\n--------------------------------------------\n"
  case parse g0 empty "S" s of
    Left e -> putStrLn e
    Right rs -> do
      forM_ (toList rs) pprint
      let try =  "ababaababa"
      putStrLn $ "Try to parse \"" <> try <> "\" with last parsed rules:\n--------------------------------------------\n"
      print $ parse rs empty "a" try

