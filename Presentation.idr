module Presentation

import Control.Linear.LIO
import Utils

-- dup : (1 _ : Int) -> (Int,Int)
-- dup x = (x,x)

-- greetAudience : (1 _ : String) -> String
-- greetAudience city = "Hello Sydney"

picture : (url : String) -> Image
picture url = fetchImage url $ \image =>
  let croppedImage = cropImage image
      transposedImage = transposeImage croppedImage
  in transposedImage

readFile : (1 _ : String) -> IO String
readFile filename = let (>>=) = myBind in do
  file <- openFile filename
  (content # filehandler) <- read file
  closeFile filehandler `seq` pure (utf8Decode content)



-- Local Variables:
-- idris-load-packages: ("support" "prelude" "network" "lib" "base" "contrib")
-- End:
