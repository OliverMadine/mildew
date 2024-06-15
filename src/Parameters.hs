{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameters where

literalSize :: Int
literalSize = 5

combinatorsPerLeaf :: Int
combinatorsPerLeaf = 5

combinatorSize :: Int
combinatorSize = 5

inputsPerAnyCharConstraint :: Int
inputsPerAnyCharConstraint = 10

charsPerSatisfyPredicate :: Int
charsPerSatisfyPredicate = 5

manyMaxSize :: Int
manyMaxSize = 5

data MyData a = MyData a | MyList a | MyString a

-- Define the type class
class MyClass a where
    myMethod :: a -> MyData a

-- General instance for all types
instance {-# OVERLAPPABLE #-} MyClass a where
    myMethod = MyData

instance {-# OVERLAPPING #-} MyClass [a] where
    myMethod = MyList

-- Specialized instance for Char
instance {-# OVERLAPPING #-} MyClass Char where
    myMethod = MyList

abc :: a -> [MyData a]
abc a = [myMethod a]

