{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Type.Equality (testEquality)
import Data.Typeable (Typeable, (:~:) (Refl))
import Type.Reflection (SomeTypeRep (..))
import Type.Reflection qualified

type FrameList a = [a]

data SomeFrameList where
  MakeSomeFrameList :: (Typeable a) => {frameList :: FrameList a} -> SomeFrameList

type ColumnName = Text

data DataFrame = DataFrame
  { columns :: Map Text SomeFrameList
  }

data ApplyError
  = ColumnDoesNotExist ColumnName
  | -- note: 'SomeTypeRep' from 'Type.Reflection' corresponds to 'TypeRep' from 'Data.Typeable'.
    -- The 'TypeRep' from 'Type.Reflection' is statically indexed by the type, the one from 'Data.Typeable' doesn't.
    TypeMismatch SomeTypeRep SomeTypeRep

apply :: forall b. (Typeable b) => DataFrame -> ColumnName -> (b -> b) -> Either ApplyError DataFrame
apply DataFrame {columns} columnName f =
  DataFrame <$> Map.alterF alteration columnName columns
  where
    alteration :: Maybe SomeFrameList -> Either ApplyError (Maybe SomeFrameList)
    alteration = \case
      Nothing -> Left $ ColumnDoesNotExist columnName
      -- The type annotation in the match binding the hidden type of tye elements into scope
      Just (MakeSomeFrameList {frameList = oldFrameList :: [a]}) ->
        -- We get the *indexed* type 'TypeRep's of the types we want to compare.
        -- It's not enough that we use the unindexed
        -- Type.Reflection.SomeTypeRep / Data.Typeable.TypeRep (same thing)
        -- because the 'testEquality' function needs the indexed versions.
        let repb :: Type.Reflection.TypeRep b = Type.Reflection.typeRep @b
            repa :: Type.Reflection.TypeRep a = Type.Reflection.typeRep @a
         in case repa `testEquality` repb of
              Nothing ->
                Left $
                  TypeMismatch (SomeTypeRep repb) (SomeTypeRep repa)
              -- By the magic of matching on the evidence :~: that the types are equal,
              -- GHC *knows* that 'a' and 'b' are the same type, while inside the pattern match.
              Just Refl ->
                Right $
                  Just $
                    MakeSomeFrameList
                      { frameList = map f oldFrameList
                      }

main :: IO ()
main = putStrLn "Hello, Haskell!"
