{-# OPTIONS -Wall -Werror #-}

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }

person :: Person
person = Person {firstName = "taro", lastName = "yamada", age = 23}
