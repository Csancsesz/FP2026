atlagTu ls = [(nev, atlag jegyek) | (nev, jegyek)<- ls]
    where
        atlag ls2= sum ls2 / fromIntegral (length ls2)
atlagTu2 ls =mapM_(\(nev, atlagJegyek)->putStrLn(nev ++ " " ++ show atlagJegyek))ls2
    where
        ls2=[(nev, atlag jegyek) | (nev, jegyek)<- ls]
        atlag ls2 = sum ls2 / fromIntegral (length ls2)
main = do
    let lsNevJegy = [("mari",[10,6,5.5,8]),("feri",[8.5,9.5]),("zsuzsa",[4.5,7.9,10]),("levi",[8.5, 9.5, 10, 7.5])]
    mapM_ (\(nev,atlagJegyek)->putStrLn(nev <> " " <> show atlagJegyek)) (atlagTu lsNevJegy)
    putStrLn "\nCsak meghivas\n"
    atlagTu2 lsNevJegy

--next lab

osztok x = [i | i <- [1..x]]

primszam x = [1,x] == osztok x
primszamOsszeg ls = sum . filter primszam $ ls 



main1= do
    putStr "x1="
    x1 <- readLn :: IO Int
    putStr "x2="
    x2 <- readLn :: IO Int
    putStrLn ("x1=" ++ show x1 ++ ", x2=" ++ show x2)
    let ls= if x1<x2 then [x1..x2] else [x2..x1]
    -- let voLs= (legtobbVO ls)
    putStrLn "Lista elemei: "
    print ls
    putStrLn ("primszamosszeg= " ++ show (primszamOsszeg ls))
    putStrLn ("listaelem osszege= " ++ show (sum ls))
    -- putStrLn ("valodi oszto= " ++ show ((snd. head)voLs) ++ ", ezzel rendelkezo szamok" ++ show ls)
    -- print (map fst voLs)


main2=do
    putStr "x1="
    x1 <-getLine
    putStr "x2="
    x2 <-getLine
    let x1Szam = read x1 :: Int
        x2Szam = read x2 :: Int
    putStrLn("x1=" ++ show x1 ++ ", x2=" ++ show x2)
    putStrLn("x1=" ++ show x1Szam ++ ", x2=" ++ show x2Szam)