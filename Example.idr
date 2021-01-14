module Example

import Data.Nat
import Data.SortedSet

data Image = MkImage

fetchImage : (url : String) -> (1 _ : ((1 _ : Image) -> Image)) -> Image
cropImage : (1 _ : Image) -> Image
transposeImage : (1 _ : Image) -> Image

picture : (url : String) -> Image
picture url = fetchImage url $ \image =>
  let croppedImage = cropImage image
      transposedImage = transposeImage croppedImage
  in transposedImage


fetchImage2 : (1 url : String) -> Image
bind : (1 _ : Image) -> (1 _ : (res -> Image)) -> Image
bindN : Image -> (res -> Image) -> Image

picture2 : (url : String) -> Image
picture2 url = let (>>=) = bind in do
  fetchImage url $ \image => do
    image2 <- cropImage image
    image3 <- cropImage image2
    image4 <- cropImage image2
    ?aukko
