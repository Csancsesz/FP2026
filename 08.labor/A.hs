import Data.List (sort,elemIndex,sortOn,group)
import Data.List (intercalate)
import Data.Char (isDigit)

varosokNagyobbMint :: Int -> [(String, Int)] -> IO ()
varosokNagyobbMint n lista = do
  let eredmeny = sort [nev | (nev, lakos) <- lista, lakos > n]

  if null eredmeny
    then putStrLn ("Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos.")
    else do
      putStrLn ("A(z) " ++ show n ++ " nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:")
      mapM_ (\v -> putStrLn ("- " ++ v)) eredmeny

--2

nincsBenneNulla :: [Int] -> IO ()
nincsBenneNulla lista = do
  let eredmeny = [x | x <- lista, not (elem '0' (show x))]

  if null eredmeny
    then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
    else do
      putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
      putStrLn (intercalate " " (map show eredmeny))

--3 

szamjegyekNelkul :: [String] -> IO ()
szamjegyekNelkul lista = do
  let szurtLista = [s | s <- lista, not (any isDigit s)]
  let rendezett = sort szurtLista
  if null rendezett
    then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
    else do
      putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
      mapM_ putStrLn rendezett

--4 

szomszedok :: String -> [String] -> IO ()
szomszedok s lsS = do
  let rendezett = sort lsS

  case elemIndex s rendezett of
    Nothing -> putStrLn "A megadott szo nincs a listaban."
    Just i -> do
      let balSzomszed = if i > 0 then Just (rendezett !! (i - 1)) else Nothing
      let jobbSzomszed = if i < length rendezett - 1 then Just (rendezett !! (i + 1)) else Nothing

      putStr (s ++ " ")

      case (balSzomszed, jobbSzomszed) of
        (Just b, Just j) ->
          putStrLn ("baloldali szomszedja " ++ b ++ ", jobboldali szomszedja pedig " ++ j)
        (Just b, Nothing) ->
          putStrLn ("baloldali szomszedja " ++ b ++ ", nincs jobboldali szomszedja.")
        (Nothing, Just j) ->
          putStrLn ("nincs baloldali szomszedja, jobboldali szomszedja pedig " ++ j)
        (Nothing, Nothing) ->
          putStrLn "a lista egyetlen eleme, nincs szomszedja."

--5

telefonStatisztika :: [(String, Int, Int)] -> IO ()
telefonStatisztika lista = do
  if null lista
    then putStrLn "A lista ures."
    else do
      let maxEladas = maximum [eladas | (_, eladas, _) <- lista]
      let legjobbak = sort [marka | (marka, eladas, _) <- lista, eladas == maxEladas]
      putStrLn ("A maximalis eladasi ertek " ++ show maxEladas ++ ".")
      putStrLn "A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:"
      mapM_ (\m -> putStrLn ("- " ++ m)) legjobbak
--6
paratlanElofordulas :: [Int] -> IO ()
paratlanElofordulas lista = do
    let szamlalt = map (\csoport -> (length csoport, head csoport)) (group (sort lista))
    let paratlanok = filter (\(db, _) -> odd db) szamlalt
    let rendezett = sortOn fst paratlanok
    if null rendezett
        then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
        else mapM_ (\(db, ertek) -> 
            putStrLn ("Elofordulas: " ++ show db ++ " -> Ertek: " ++ show ertek)) rendezett

            
main :: IO ()
main = do
  let varosAdatok =
        [ ("sepsiszentgyorgy", 54000),
          ("kolozsvár", 330000),
          ("marosvasarhely", 130000),
          ("temesvar", 310000),
          ("arad", 160000),
          ("gyergyoszentmiklos", 18000),
          ("nagyvarad", 196000)
        ]
  varosokNagyobbMint 150000 varosAdatok
  nincsBenneNulla [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
  let bemenet = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
  szamjegyekNelkul bemenet
  let s = "feri"
  let lsS = words "Mari Zsuzsa szidi Lori kata feri teri Dani zsolti"
  szomszedok s lsS
  let adatok =  [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
  telefonStatisztika adatok
  paratlanElofordulas [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]