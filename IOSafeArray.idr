module IONDSafeArray

import Data.Fin

{-- This file implements multidimensional safe arrays --}

data ArrayData : Type -> Type where
export
data SDArray l e = MkSDArray Int (ArrayData e)

export
data NDArray : (shape : List Int) -> Type -> Type where
     NDAZ : (value : t) -> NDArray [] t
     NDA  : (values : SDArray Int (NDArray s t)) -> NDArray (n::s) t

export
newArray : (len : Int) -> e -> IO (SDArray Int e)
newArray size default
         = do vm <- getMyVM
              MkRaw p <- foreign FFI_C "idris_newArray"
                             (Ptr -> Int -> Raw e -> IO (Raw (ArrayData e)))
                             vm size (MkRaw default)
              pure (MkSDArray size p)

intToFin : Int -> (n : Nat) -> Maybe (Fin n)
intToFin x Z = Nothing
intToFin x n = if x >= 0
               then natToFin (cast x) n
               else Nothing

natToInt : Nat -> Int
natToInt n = cast n

unsafeWriteSDArray : SDArray l e -> Nat -> e -> IO ()
unsafeWriteSDArray (MkSDArray size p) i val
                 = let n = natToInt i in
                   foreign FFI_C "idris_arraySet"
                   (Raw (ArrayData e) -> Int -> Raw e -> IO ())
                   (MkRaw p) n (MkRaw val)

unsafeReadSDArray : SDArray l e -> Nat -> IO e
unsafeReadSDArray (MkSDArray size p) i
                = do let n = natToInt i
                     MkRaw val <- foreign FFI_C "idris_arrayGet"
                                          (Raw (ArrayData e) -> Int -> IO (Raw e))
                                          (MkRaw p) n
                     pure val

export
readSDArray : Fin l -> (arr : SDArray Nat e) -> IO e
readSDArray FZ arr = unsafeReadSDArray arr Z
readSDArray (FS k) arr = readSDArray k arr

export
writeSDArray : Fin l -> (arr : SDArray Nat e) -> e -> IO ()
writeSDArray FZ arr e = unsafeWriteSDArray arr Z e
writeSDArray (FS k) arr e = writeSDArray k arr e
