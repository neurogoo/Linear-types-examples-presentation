module Linear2

data Item = MkItem

data ShoppingCart : List Item -> Type where
  NewCart : ShoppingCart []
  AddItem : (i : Item) -> ShoppingCart l -> ShoppingCart (i :: l)
  Checkout : (1 _ : ShoppingCart l) -> ShoppingCart []
  Bind : (ShoppingCart a) -> (item -> ShoppingCart b) -> ShoppingCart b

(>>=) : (ShoppingCart a) -> (item -> ShoppingCart b) -> ShoppingCart b
(>>=) = Bind

shoppingTrip : ShoppingCart []
shoppingTrip = do
  newCart <- NewCart
  cart <- AddItem MkItem ?aukko1
  Checkout cart
  ?aukko
