module IONDSafeArray

{-- This file implements multidimensional safe arrays --}

data ArrayData : Type -> Type where
export
data SDArray l e = MkSDArray Int (ArrayData e)

export
data NDArray : (shape : List Int) -> Type -> Type where
     NDAZ : (value : t) -> NDArray [] t
     NDA  : (values : SDArray Int (NDArray s t)) -> NDArray (n::s) t


newArray : (len : Int) -> e -> IO (SDArray Int e)
newArray size default
         = do vm <- getMyVM
              MkRaw p <- foreign FFI_C "idris_newArray"
                             (Ptr -> Int -> Raw e -> IO (Raw (ArrayData e)))
                             vm size (MkRaw default)
              pure (MkSDArray size p)

safeWriteSDArray : SDArray l e -> Int -> e -> IO ()
safeWriteSDArray (MkSDArray size p) i val
                 = foreign FFI_C "idris_arraySet"
                   (Raw (ArrayData e) -> Int -> Raw e -> IO ())
                   (MkRaw p) i (MkRaw val)

safeReadSDArray : SDArray l e -> Int -> IO e
safeReadSDArray (MkSDArray size p) i
                = do MkRaw val <- foreign FFI_C "idris_arrayGet"
                                          (Raw (ArrayData e) -> Int -> IO (Raw e))
                                          (MkRaw p) i
                     pure val
