module Main

import IOSafeArray

main : IO (SDArray Int String)
main = newArray 5 "a"

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
