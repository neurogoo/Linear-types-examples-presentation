module Utils

public export
data Image = mkImage

public export
fetchImage : (url : String) -> (1 _ : ((1 _ : Image) -> Image)) -> Image
fetchImage = believe_me

public export
cropImage : (1 _ : Image) -> Image
cropImage image = image

public export
transposeImage : (1 _ : Image) -> Image
transposeImage image = image

data FileHandle = MkFileHandle

public export
openFile : (1 _ : String) -> IO FileHandle

public export
read : (1 _ : FileHandle) -> IO (LPair String FileHandle)

public export
closeFile : (1 _ : FileHandle) -> IO ()

public export
seq : (1 _ : IO ()) -> IO String -> IO String

public export
utf8Decode : String -> String

public export
myBind : (1 _ : IO a) -> (1 _ : ((1 _ : a) -> IO b)) -> IO b
