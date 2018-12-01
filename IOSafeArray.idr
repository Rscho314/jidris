module IONDSafeArray

{-- This file implements multidimensional safe arrays --}

data ArrayData : Type -> Type where
export
data SDArray l el = MkSDArray Int (ArrayData el)

export
{-data NDArray : (shape : List Int) -> Type -> Type where
     NDAZ : (value : t) -> NDArray [] t
     NDA  : (values : SDArray n (NDArray s t)) -> NDArray (n::s) t
-}
newArray : (len : Int) -> e -> IO (SDArray Int e)
newArray size default
         = do vm <- getMyVM
              MkRaw p <- foreign FFI_C "idris_newArray"
                             (Ptr -> Int -> Raw e -> IO (Raw (ArrayData e)))
                             vm size (MkRaw default)
              pure (MkSDArray size p)
