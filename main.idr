module Main

--import IOSafeArray
import IOSafeArray
import Data.Vect

main : IO ()
main = do a <- newArray 5 "a"
          b <- readSDArray 1 a
          print b

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
