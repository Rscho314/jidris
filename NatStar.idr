module NatStar

data NatStar = O
             | S NatStar

total
addNumStar : (n, m : NatStar) -> NatStar
addNumStar O right        = S right
addNumStar (S left) right = S (addNumStar left right)

total
mulNumStar : NatStar -> NatStar -> NatStar
mulNumStar O right        = right
mulNumStar (S left) right = addNumStar right $ mulNumStar left right

total
fromIntegerNatStar : Integer -> NatStar
fromIntegerNatStar 1 = O
fromIntegerNatStar n =
                   if (n > 1)
                   then S (fromIntegerNatStar (assert_smaller n (n - 1)))
                   else O

Num NatStar where
    (+) = addNumStar
    (*) = mulNumStar
    fromInteger x = fromIntegerNatStar x

total
toIntegerNatStar : NatStar -> Integer
toIntegerNatStar O = 1
toIntegerNatStar (S k) = 1 + toIntegerNatStar k

||| Note that this can overflow
total
toIntNatStar : NatStar -> Int
toIntNatStar n = prim__truncBigInt_Int (toIntegerNatStar n)

Cast NatStar Integer where
     cast = toIntegerNatStar

Cast NatStar Int where
     cast = toIntNatStar

Cast Integer NatStar where
     cast = fromInteger

Cast Int NatStar where
     cast i = fromInteger (cast i)

Show NatStar where
     show O = show (the Integer (cast O))
     show (S k) = show (the Integer (cast (S k)))

