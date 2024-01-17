{-# language GADTs #-}
module Main where

import Data.Typeable (Typeable,  (:~:)(Refl) )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Type.Reflection (SomeTypeRep (..)) 
import Type.Reflection qualified
import Data.Type.Equality (testEquality)


type FrameList a = [a]

data SomeFrameList where
    MakeSomeFrameList :: Typeable a => { frameList :: FrameList a } -> SomeFrameList

type ColumnName = Text

data DataFrame = DataFrame {
    columns :: Map Text SomeFrameList
}

data ApplyError = 
          ColumnDoesNotExist ColumnName
          -- note: 'SomeTypeRep' from 'Type.Reflection' corresponds to 'TypeRep' from 'Data.Typeable'.
          -- The 'TypeRep' from 'Type.Reflection' is statically indexed by the type, the one from 'Data.Typeable' doesn't.
        | TypeMismatch SomeTypeRep SomeTypeRep

apply :: forall b . Typeable b => DataFrame -> ColumnName -> (b -> b) -> Either ApplyError DataFrame
apply DataFrame {columns} columnName f =
  case Map.lookup columnName columns of 
    Nothing -> Left $ ColumnDoesNotExist columnName
    -- The type annotation brings the type into scope
    Just (MakeSomeFrameList {frameList = oldFrameList :: a}) -> do
        let repb = Type.Reflection.typeRep @b 
        let repa = Type.Reflection.typeRep @a 
        case repa `testEquality` repb of
            Nothing -> Left $ TypeMismatch (SomeTypeRep repb) (SomeTypeRep repa)
            -- By the magic of pattern matching on the evidence :~: that the types are equal,
            -- inside this patterns match, GHC *knows* that 'a' and 'b' are the same type.
            Just Refl -> 
                let newFrameList = f oldFrameList
                    newColumns = Map.insert columnName (MakeSomeFrameList newFrameList) columns    
                    newDataFrame = DataFrame { columns = newColumns} 
                 in Right newDataFrame
                




main :: IO ()
main = putStrLn "Hello, Haskell!"
