module IONDArray

import Data.Fin

-- TODO: refactor to multidimensional case and immutable interface (with mutable underneath)
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
--}
total
sdArraySize : SDArray l e -> Nat
sdArraySize arr = l

total
WriteSDArray : SDArray l e -> (i : Nat) -> {auto prf : (i < l) && (i > 0) = True} -> e -> IO ()
WriteSDArray (MkSDArray size p) idx val
                 = let n : Int = cast idx in
                   foreign FFI_C "idris_arraySet"
                   (Raw (ArrayData e) -> Int -> Raw e -> IO ())
                   (MkRaw p) n (MkRaw val)



total
ReadSDArray : SDArray l e -> (i : Nat) -> {auto prf : (i < l) && (i > 0) = True} -> IO e
ReadSDArray (MkSDArray size p) idx
            = do let n : Int = cast idx
                 MkRaw val <- foreign FFI_C "idris_arrayGet"
                       (Raw (ArrayData e) -> Int -> IO (Raw e))
                       (MkRaw p) n
                 pure val

example : IO Int
example = do arr <- newSDArray 5 1
             WriteSDArray arr 4 0
             i <- ReadSDArray arr 1
             pure i
