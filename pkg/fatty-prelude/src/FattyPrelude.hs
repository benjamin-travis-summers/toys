{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module FattyPrelude
    ( module ClassyPrelude.Conduit
    , module Turtle
    , module Control.Lens
    , module RIO
    , makeFields
    ) where

import ClassyPrelude.Conduit
  hiding (log, bracket, FilePath, (<.>), (</>))

import Turtle
  hiding (view, find, stripPrefix, sortOn, (<>), sort, sortBy, stderr, stdin, stdout,
          fork, fold, Fold, contains, has, noneOf, strict)

import Control.Lens
  hiding (Index, (<.>), (<|), index, uncons, unsnoc, cons, snoc, lensRules, makeLenses, makeFields)

import RIO (RIO, runRIO, bracket)

--------------------------------------------------------------------------------------------------------------

import qualified Control.Lens.TH     as Lens
import qualified Data.Char           as C
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------------------------------------

simplifiedLensName :: Lens.DefName -> Lens.DefName
simplifiedLensName = \case Lens.MethodName _cls fld -> Lens.TopName fld
                           Lens.TopName fld         -> Lens.TopName fld

overHead :: (a -> a) -> [a] -> [a]
overHead _ []     = []
overHead f (x:xs) = f x : xs

fieldLabelModifier :: TH.Name -> TH.Name -> String
fieldLabelModifier (TH.nameBase -> typeName) (TH.nameBase -> fieldName) =
    fromMaybe badFieldErr modifiedFieldName
  where
    prefix = fmap C.toLower (filter C.isUpper typeName)

    invalidNm = "Field `" <> fieldName <> "` has an invalid name."

    explaination = concat [ "Fields for type `", typeName, "` must be prefixed by '"
                          , prefix
                          , "' or '"
                          , "_" <> prefix
                          , "' followed by an upper case character."
                          ]

    badFieldErr = error (invalidNm <> " " <> explaination)

    modifiedFieldName = do
        result <- stripPrefix prefix fieldName <|> stripPrefix ("_" <> prefix) fieldName
        maybe mzero (guard . C.isUpper) (headMay result)
        return (overHead C.toLower result)

lensNamer :: TH.Name -> [TH.Name] -> TH.Name -> [Lens.DefName]
lensNamer typeNm _allFields fieldNm =
    [Lens.MethodName (TH.mkName clsNm) (TH.mkName methodNm)]
  where
    slotNm   = fieldLabelModifier typeNm fieldNm
    clsNm    = "Has" <> overHead C.toUpper slotNm
    methodNm = (TH.nameBase fieldNm) <> "L"

fieldRules :: Lens.LensRules
fieldRules = Lens.camelCaseFields & Lens.lensField .~ lensNamer

lensRules :: Lens.LensRules
lensRules = Lens.lensRules & Lens.lensField .~ (\ty flds fld -> simplifiedLensName <$> lensNamer ty flds fld)

makeFields :: TH.Name -> TH.DecsQ
makeFields = Lens.makeLensesWith fieldRules
