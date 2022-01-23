#!/usr/bin/env runghc

import Control.Monad

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


pshow :: (String,Grammar String) -> String
pshow (name,g) = unlines . (\(_,_,_,i)->i) . top name . go id $ g
  where
    go f (Atom Ter t) = box $ f $ "'" ++ t ++ "'"
    go f (Atom NTer t) = box $ f $ t
    go f GTer = box $ f $ "ELTER"
    go f GNTer = box $ f $ "IDNTER"
    go f (Err e) = box $ f $ "Error: " ++ show e
    go f (a :. b) = br (f ".") (go id a) (go id b)
    go f (a :+ b) = br (f "+") (go id a) (go id b)
    go f (Star a) = go (\s->"["++s++"]") a
    go f (Un a) = go (\s->"(|"++s++"|)") a
    go f (a :# i) = go (++("#"++show i)) a

pprint = putStrLn . pshow
-- main

main = do
  let g0 =  [("S",(n "N" :. t "->" :. n "E" :. t "," :# 1) :. t ";")
            ,("N",GNTer :# 2)
            ,("E",n "T" :. s (t "+" :. n "T" :# 3))
            ,("T",n "F" :. s (t "+" :. n "F" :# 4))
            ,("F",GNTer :# 5 :+ GTer :# 5 :+ t "(" :. n "E" :. t ")" :+ t "[" :. n "E" :. t "]" :+ t "(|" :. n "E" :. t "|)")
            ]
  forM_ g0 pprint
