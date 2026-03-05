szjOsszeg n
    | n<0 = szjOsszeg(abs n)
    | n<10 = n
    | otherwise = mod n 10 + szjOsszeg(div n 10)

szjOsszeg2 n res
  | n < 0 = szjOsszeg2 (abs n) res
  | n < 10 = res + n
  | otherwise = szjOsszeg2 (div n 10) (res+mod n 10)

szjSzam n res
    | n< 0 =szjSzam (abs n) res
    |n < 10 = res+1
    | otherwise = szjSzam (div n 10) (res + 1)


szjSzam2 n
  | n < 0 = szjSzam2 (abs n)
  | n < 10 = 1
  | otherwise = 1+ szjSzam2 (div n 10)

fugv4 n szj
    |szj>9 = error "nem szamjegy tesomsz"
    |n< 10 = if n == szj then szj else 0
    |otherwise = if mod n 10 == szj then szj+ fugv4 (div n 10) szj else fugv4 (div n 10) szj

pSzJ n
    | n<0 = pSzJ (abs n)
    |n<10 = if even n then 1 else 0
    |otherwise =
        if even(mod n 10)
            then 1 + pSzJ(div n 10)
            else pSzJ(div n 10)

lnSzj n ln
    | n<0 =  lnSzj ( abs n) ln
    | n<10 = if n > ln then n else ln
    | otherwise= if mod n 10 > ln
        then lnSzj (div n 10) (mod n 10)
        else lnSzj (div n 10) ln

bSzamrDSzj n b d
    | n < 0 = bSzamrDSzj (abs n) b d
    | n< b = if n == d then 1 else 0
    | otherwise =
        if mod n b == d
            then 1+ bSzamrDSzj (div n b) b d
            else bSzamrDSzj (div n b) b d

fibo a b res n
    | n  == 0 = res
    | otherwise = fibo b res (res + b) (n - 1)

fiboN n = fibo 0 1 0 n

fiboLs n = map (fibo 0 1 0) [0 .. n]

ls1=[123, 31, 41 ,64]
szjOsszegLs= map szjOsszeg ls1

szjOsszegLs2= map (\x->szjOsszeg2 x 0) ls1

szjSzamLs ls =map szjSzam2 ls

ls2=[(577723707, 7), (423,3),(0,1),(12,2)]
szjSzamOsszegLs ls = map (uncurry fugv4)
 ls
szjSzamOsszegLs2 ls = map (\(x,szj) -> fugv4 x szj)ls

--lab3 cuccok

atlag ls = (sum ls) / fromIntegral (length ls)
