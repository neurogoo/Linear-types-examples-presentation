module Linear

import Data.Nat
import Prelude
import Data.DPair

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

read : (1 _ : String) -> LPair String String
read filename = "joo" # filename

closeFile : (1 _ : String) -> Bool

readFile : (1 _ : String) -> String
readFile filename =
  let (content # filehandler) = read filename
      hah = closeFile filehandler
  in if hah then content else content

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
