module Linear

import Data.Nat
import Prelude
import Data.DPair
import Control.Linear.LIO

-- small example from my previous talk 1.5 years ago

goodPassword : String -> Type
goodPassword s = length s `GTE` 12

record Login where
  constructor MkLogin
  loginName : String
  password : (s : String ** goodPassword s)

passwordStrengthChecker : (password : String) -> Dec (goodPassword password)
passwordStrengthChecker password = isLTE 12 (length password)

createNewUser : (loginName : String) -> (password : String) -> Either String Login
createNewUser loginName password =
  case passwordStrengthChecker password of
    Yes prf => Right $ MkLogin loginName (password ** prf)
    No _    => Left "Too short password"

--testNewUserCreation : Either String Login
--testNewUserCreation = createNewUser "hee" "jeedfjaisfasdjopfjaspfs"

-- dup : (1 _ : Int) -> (Int,Int)
-- dup x = (x,x)

-- greetAudience : (1 _ : String) -> String
-- greetAudience city = "Hello Sydney"

-- lprint : (1 _ : List Char) -> IO ()
-- lprint [] = pure ()
-- lprint (s :: ss) = do
--   print s
--   lprint ss

-- linearPrint : (1 _ : String) -> IO ()
-- linearPrint s = ?heh

data FileHandle = MkFileHandle

openFile : (1 _ : String) -> IO FileHandle

read : (1 _ : FileHandle) -> IO (LPair String FileHandle)

closeFile : (1 _ : FileHandle) -> IO ()

seq : (1 _ : IO ()) -> IO String -> IO String

utf8Decode : String -> String

readFile : (1 _ : String) -> IO String
readFile filename = let (>>=) = bindL in do
  file <- openFile filename
  (content # filehandler) <- read file
  closeFile filehandler `seq` pure (utf8Decode content)

data Token = MkToken

data AutenticationStatus = Autenticated
                         | NotAutenticated

data SecretData : Type where
  MkSecret : (1 _ : String) -> SecretData
  NullData : SecretData

checkAutentication : Token -> AutenticationStatus
checkAutentication MkToken = Autenticated

data Secret : AutenticationStatus -> SecretData -> Type where
  CheckAutentication : (t : Token) -> Secret (checkAutentication t) NullData
  ReadSecret         : (1 _ : Token) -> Secret a NullData
  (>>=)              : Secret a b -> (v -> Secret c d) -> Secret e f


test : Secret NotAutenticated NullData
test = do
  joku <- CheckAutentication MkToken
  jotain <- ReadSecret joku
  ?hah

-- Local Variables:
-- idris-load-packages: ("support" "prelude" "network" "lib" "base" "contrib")
-- End:
