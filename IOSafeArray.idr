module IONDSafeArray

import Data.Fin

{-- This file implements multidimensional safe arrays --}

-- TODO: refactor to multidimensional case, implement Show interface
total
notNatZero : (n : Nat) -> {auto prf : n > 0 = True} -> Nat
notNatZero n = n

data ArrayData : Type -> Type where
export
data SDArray : (len : Nat) -> elem -> Type where
     MkSDArray : (l : Nat) -> {auto prf : l > 0 = True} -> (ArrayData el) -> (SDArray len el)

export
data NDArray : (shape : List Nat) -> Type -> Type where
     MkNDArray  : (values : SDArray n (NDArray s t)) -> NDArray (n::s) t

total
intToFin : Int -> (n : Nat) -> Maybe (Fin n)
intToFin x Z = Nothing
intToFin x n = if x >= 0
               then natToFin (cast x) n
               else Nothing

export
total
newSDArray : (l : Nat) -> {auto prf : l > 0 = True} -> e -> IO (SDArray l e)
newSDArray size default
         = do let s : Int = cast size
              vm <- getMyVM
              MkRaw p <- foreign FFI_C "idris_newArray"
                             (Ptr -> Int -> Raw e -> IO (Raw (ArrayData e)))
                             vm s (MkRaw default)
              pure (MkSDArray size p)

{--
newNDArray : (s : List Nat) -> e -> IO (NDArray s e)
newNDArray shape default
           = let ss = map natToInt shape in
             do
             pure (MkNDArray )

total
unsafeWriteSDArray : SDArray l e -> Nat -> e -> IO ()
unsafeWriteSDArray (MkSDArray size p) idx val
                 = let n = natToInt idx in
                   foreign FFI_C "idris_arraySet"
                   (Raw (ArrayData e) -> Int -> Raw e -> IO ())
                   (MkRaw p) n (MkRaw val)

total
unsafeReadSDArray : SDArray l e -> Nat -> IO e
unsafeReadSDArray (MkSDArray size p) idx
                = do let n = natToInt idx
                     MkRaw val <- foreign FFI_C "idris_arrayGet"
                                          (Raw (ArrayData e) -> Int -> IO (Raw e))
                                          (MkRaw p) n
                     pure val

export
partial
readSDArray : Fin l -> (arr : SDArray l e) -> IO e
readSDArray FZ arr = unsafeReadSDArray arr Z
readSDArray (FS k) arr = readSDArray FZ arr

export
partial
writeSDArray : Fin l -> (arr : SDArray l e) -> e -> IO ()
writeSDArray FZ arr e = unsafeWriteSDArray arr Z e
writeSDArray (FS k) arr e = writeSDArray FZ arr e
--}
 
