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
createNewUser loginName password = case passwordStrengthChecker password of
  Yes prf => Right $ MkLogin loginName (password ** prf)
  No _  => Left "Password was not long enough"

testNewUserCreation : Either String Login
testNewUserCreation = createNewUser "hee" "jeedfjaisfasdjopfjaspfs"

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
