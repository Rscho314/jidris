module Main

--import IOSafeArray
import IOSafeArray
import Data.Vect

main : IO ()
main = do a <- (the NatStar 5)
          print a

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
