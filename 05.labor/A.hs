import Data.Foldable1 (Foldable1(fold1))
import Distribution.Simple.PreProcess (ppC2hs)
type Pont =(Double, Double)

lsP :: [Pont]
lsP = [(4.5,6.1),(1.2,3.4), (6,8),(4.5,2.4)]

p :: Pont
p= (3.4, 5.6)

tavolsag(x1,y1)(x2,y2)=sqrt ((x1-x2)**2+(y1-y2)**2)

minTavolsag lsP p =fold1(\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP
minTavolsag2 lsP p=fold1 aux lsP
    where   
        aux p1 p2 =if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 [p1]_=p1
minTavolsag3 (p1 : p2 : ls)p
    | tavolsag p1 p <tavolsag p2 p =minTavolsag3(p1 : ls)p
    | otherwise = minTavolsag3(p2 : ls)p

parosNegyzet n = [i ^2 | i<- [0,2..n*2]]

parosNegyzet2 n=take n [i^2 | i<- [0,2 ..]]

szamokLs n
    | n==1 = replicate n n
    | otherwise = szamokLs (n-1)++ replicate n n

szamokLs2 n i
    |i/= n = replicate i i ++ szamokLs2 n (i+1)
    |otherwise = replicate i i

szamokLs3 n i
    | i/=n =replicate i (i*2) ++ szamokLs3 n (i+1)
    | otherwise = replicate i (i*2)

lsN n = [n,n-1 .. 1] ++ [1 .. n]

lsN2 n = reverse [1 .. n] ++ [1 .. n]

tf n = [even i | i<-[0..n]]

tf2 n =take n ls
    where
        ls= [True,False] ++ ls

tf3 n = concat $ replicate n [True,False]

nullEgyMinEgy n = take n ls
  where
    ls = [0,1,-1] ++ ls

osztok n= [i | i <- [1.. n], mod n i ==0]

osztokSz n = length $ osztok  n

osztokSz2 n = myLength $ osztok n
    where
        myLength []=0
        myLength(_:ls) = 1+myLength ls

osztokSz3 n = fold1 (\res i -> if mod n i == 0 then res + 1 else res) 0 [1..n]

lnP n = maximum [i | i <-osztok n, odd i]

lnP2 n = last [i | i <- [1,3 .. div n 2], mod n i == 0]

decP x p
    | x< 0 = error "neg szam"
    | x< p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p=length $ decP x p

decPSzam2 x p = myLength $ decP x p
  where
    myLength [] = 0
    myLength (_ : ls) = 1 + myLength ls

myLength2 []=0
myLength2(_:ls)=1+myLength2 ls

decPSzam3 x p = myLength2 $ decP x p

decPMax x p = maximum $ decP x p

fibo = fiboSg 0 1 0
    where
        fiboSg a b res=res: fiboSg b res (res+b)

fiboAB a b =(dropWhile(<a) . takeWhile (<b)) fibo


atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)


pozAtlag ls = atlag [i | i <- ls, i>0]

pozAtlag2 ls = atlag $ filter (>0) ls

listaNElem ls n = [i | (idx, i) <- zip [1..] ls , mod idx n ==0]

listaNElem2 ls n i
    | i >= length ls = []
    | mod i n == 0 = ls !! i : listaNElem2 ls n (i+1)
    | otherwise = listaNElem2 ls n (i+1)

tukor ls= reverse ls

tukor2 ls = foldl (\res x -> x : res) [] ls

tukor3 ls = map tukorSzam ls
  where
    tukorSzam 0 = []
    tukorSzam x = mod x 10 : tukorSzam (div x 10)

