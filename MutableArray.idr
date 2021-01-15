module MutableArray

data MArray a = MkM
data Array a = MkA
data Ur : Type -> Type where
  MkUr : a -> Ur a

newMArray : Int -> (1 _ : ((1 _ : MArray a) -> Ur b)) -> b
write : (1 _ : MArray a) -> (Int, a) -> MArray a
read : (1 _ : MArray a) -> Int -> (MArray a, Ur a)
freeze : (1 _ : MArray a) -> Ur(Array a)
foldl : ((1 _ : a) -> b -> a) -> (1 _ : a) -> (1 _ : List b) -> a

array : Int -> List (Int,a) -> Array a
array size pairs = newMArray size $ \ma =>
  freeze (foldl write ma pairs)
