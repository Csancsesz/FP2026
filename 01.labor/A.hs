x=3
y=4

z=x+y


osszeg :: Num a => a -> a -> a
osszeg a b = a+ b

kulonbseg :: Num a => a -> a -> a
kulonbseg a b= a- b

szorzat :: Num a => a -> a -> a
szorzat a b= a* b

hanyados :: Fractional a => a -> a -> a
hanyados a b= a / b

hanyados2 :: Integral a => a -> a -> a
hanyados2 a b= div a b 

osztmar :: Integral a => a -> a -> a
osztmar a b = mod a b

elsoF :: Fractional a => a -> a -> a
elsoF a b= -b / a 


elojel :: (Ord a, Num a) => a -> String
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"