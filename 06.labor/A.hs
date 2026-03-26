import Data.Char
import Control.Monad.Cont (label)

--tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)
--tokenize = map (irasjelHelyettesit . toLower)


irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

lengthLista ls = map length ls

talalat x ls= l1
        where 
            zipls= zip ls [0..]
            l1= map snd $ filter (\y-> fst y ==x) zipls

ps ls = sum [t2 | (t1,t2,t3)<- ls]

ps2 ls r = sum [t2 | (t1, t2, t3) <- ls1]
    where
        ls1= filter(\(t1,t2,t3)-> t3 == r)ls

ps3 ls r = sum [t2 | (t1, t2, t3) <- ls, t3 == r]
  
main= do
    -- let lista="ez egy PrOBA szoveg. az egy masik proBa! Tobbfele irasJEL ::Hasznalat "
    -- let l1=tokenize lista
    -- let l2=lengthLista l1
    -- putStrLn "a szavak hossza: "
    -- print l2
    -- let l3 = zip l1 l2
    -- print l3
    -- let m1= minimum l2
    -- putStrLn"a legrovidebb szavak: "
    -- let result = map fst (filter (\(_,y) -> m1 == y) l3)
    -- print result
    -- let a=5
    -- let l1= [3, 13, 5, 6, 7, 12, 5, 8, 5]
    -- let t1= talalat a l1
    -- let a2='e'
    -- let l2 ="Bigeri-vizeses"
    -- let t2= talalat a2 l2
    -- let c1=concatMap ((<> " ") . show ) t1
    -- putStrLn $ show a <> " talalat pozicioi: " <> c1 
    -- let c2=concatMap ((<> " ") . show)  t2
    -- putStrLn $ show a2 <> " talalat pozicioi: " <> c2
    let lsMadar = [("golya", 120, "ms"), ("fecske", 85, "cj"), ("cinege", 132, "ms")]
    let result=ps lsMadar
    let madarLs=concatMap (<> " ") [t1 | (t1,t2,t3)<-lsMadar]
    putStrLn $ madarLs <> "populacio szamma: " <> show result 
    let r="ms"
    let result2 = ps2 lsMadar r
    print result2
    putStrLn $ show r <> "megyeben " <> show madarLs <> " madarak populacioszama: " <> show result2
    