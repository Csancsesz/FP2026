import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
elempar ep1 ep2 = (a == c && b == d) || (a == d && b == c)
    where
        (a,b) =ep1
        (c,d) = ep2

elempar2 (a,b) (c,d) = (a == c && b == d) || (a == d && b == c)

main :: IO()
main=do
    putStr "elempar (6,7) (7,6): "
    print(elempar (6,7) (7,6))
    putStrLn ("elempar (6,7) (4,7): " ++ show (elempar (6,7) (4,7)))
    print(elempar2 (6,7) (4,7))

fakt1 0 = 1
fakt1 n=n*fakt1 (n-1)

fakt2 n
    | n<0 = error "neg szam"
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = n* fakt2 (n-1)

fakt3 n res
    | n< 0 = error "nega"
    | n == 0 = res
    | otherwise = fakt3 (n-1) (res*n) 

hatvanyX x n
    | n< 0 = error "nega"
    | otherwise = x**n

hatvanyX2 x n
    | n<0 = error "neg"
    | otherwise = x^n

hatvanyX3 x n
    | n<0 = error "nega"
    |n ==0 =1
    | otherwise = x* hatvanyX3 x (n-1)

negyzetgyok n = [sqrt i | i <- [1..n]]

negyzetszam n = [i^2 | i <- [1..n]]

kobszam n= [i^3 | i<- [1..n]]

nemNegyzet n = [i | i<- [1..n], i /=(sqrt i^2)]

hatvany x n= [x^i | i<- [1..n]]

parosOsztok x = [i | i<- [1..x],mod x i ==0,mod i 2 == 0]
parosOsztok2 x = [i | i<- [1..x],mod x i ==0,even i]

osztok x = [i | i<- [1..x],mod x i ==0]
prim x = osztok x == [1,x]
primek n =[i | i<- [1..n],prim i ==True]

primszam2 n = [i| i <-[1..n],primszamL i]
    where
        primszamL sn= [1,sn] ==osztokL sn
        osztokL sn2 =[j|j<-[1..sn2],mod sn2 j ==0]

osszetett n = [i | i <- [1 .. n], prim i == False]
osszetett2 n = [i | i <- [1 .. n], not(prim i)]

pitagorasz n= [(a,b,c) | c <- [1..n], b<-[1..c], a<-[1..b],a^2+b^2==c^2]

betuSzam = zip['a' .. 'z'] [0..25]
betuSzam2 = zip ['a' .. 'z'] [1 , 3 .. 25]

szamok = zip [0..5][5,4..0]

szamok2 n = zip [0..n][n,n-1..0]
falsetrue n= take n ls
    where
        ls=[True,False] ++ ls

falsetrue2 n =[mod i 2 ==0 | i<-[0..n-1]]

-- - lab2 hellyea
szjSzorzat 0 =1
szjSzorzat n = mod n 10 * szjSzorzat (div n 10)

szjSzorzat2 n
    | n< 0= error "nega"
    | div n 10 ==0 = n
    | otherwise = mod n 10 * szjSzorzat2 (div n 10)


szjOssz 0 =0
szjOssz n = mod n 10 + szjOssz (div n 10)

szjOssz2 n
    | n< 0= error "nega"
    | div n 10 ==0 = n
    | otherwise = mod n 10 + szjOssz2 (div n 10)

