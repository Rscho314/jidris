module IOSafeArray

import Data.IOArray

{-- This file builds on the Data.IOArray module, and implements compile-time checks --}

export
say : IO()
say = putStrLn "Pretty!"
