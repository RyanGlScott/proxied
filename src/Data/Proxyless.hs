{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-|
Module:      Data.Proxyless
Copyright:   (C) 2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Remove the 'Proxy' and 'undefined' arguments from functions with 'proxyless'
and 'undefinedless', respectively, which produce functions that take type
information via the @-XTypeApplications@ GHC extension.

This module is only available with GHC 8.0 or later.

/Since: 0.2/
-}
module Data.Proxyless (
      -- * 'proxied' and 'unproxied'
      proxyless
    , undefinedless
      -- * Proxyless functions
      -- ** "Data.Bits"
    , theBitSize
    , theIsSigned
    , theBitSizeMaybe
    , theFiniteBitSize
{-
      -- ** "Data.Data"
    , theDataTypeOf
      -- ** "Data.Typeable"
    , theTypeRep
      -- ** "Foreign.Storable"
    , theSizeOf
    , theAlignment
      -- ** "GHC.Generics"
    , theDatatypeName
    , theModuleName
    , theIsNewtype
    , thePackageName
    , theConName
    , theConFixity
    , theConIsRecord
    , theSelName
    , theSelSourceUnpackedness
    , theSelSourceStrictness
    , theSelDecidedStrictness
      -- ** "Prelude"
    , theFloatRadix
    , theFloatDigits
    , theFloatRange
      -- ** "Text.Printf"
    , theParseFormat
-}
    ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Data hiding (Fixity)
import Data.Proxy (Proxy(..))

import Foreign.Storable (Storable(..))

import GHC.Generics

import Text.Printf (PrintfArg(..), ModifierParser)

proxyless :: forall a b. (Proxy a -> b) -> b
proxyless f = f Proxy

undefinedless :: forall a b. (a -> b) -> b
undefinedless f = f undefined

-------------------------------------------------------------------------------
-- Data.Bits
-------------------------------------------------------------------------------

-- | @'theBitSize' = 'undefinedless' 'bitSize'@
--
-- /Since: 0.2/
theBitSize :: forall a. Bits a => Int
theBitSize = undefinedless @a bitSize

-- | @'theIsSigned' = 'undefinedless' 'isSigned'@
--
-- /Since: 0.2/
theIsSigned :: forall a. Bits a => Bool
theIsSigned = undefinedless @a isSigned

-- | @'theBitSizeMaybe' = 'undefinedless' 'bitSizeMaybe'@
--
-- /Since: 0.2/
theBitSizeMaybe :: forall a. Bits a => Maybe Int
theBitSizeMaybe = undefinedless @a bitSizeMaybe

-- | @'theFiniteBitSize' = 'undefinedless' 'finiteBitSize'@
--
-- /Since: 0.2/
theFiniteBitSize :: forall a. FiniteBits a => Int
theFiniteBitSize = undefinedless @a finiteBitSize

{-
-------------------------------------------------------------------------------
-- Data.Data
-------------------------------------------------------------------------------

-- | @'dataTypeOfProxied' = 'proxied' 'dataTypeOf'@
--
-- /Since: 0.2/
dataTypeOfProxied :: Data a => proxy a -> DataType
dataTypeOfProxied = proxied dataTypeOf

-------------------------------------------------------------------------------
-- Data.Typeable
-------------------------------------------------------------------------------

-- | @'typeOfProxied' = 'proxied' 'typeOf'@
--
-- /Since: 0.2/
typeOfProxied :: Typeable a => proxy a -> TypeRep
typeOfProxied = typeRep

-------------------------------------------------------------------------------
-- Foreign.Storable
-------------------------------------------------------------------------------

-- | @'sizeOfProxied' = 'proxied' 'sizeOf'@
--
-- /Since: 0.2/
sizeOfProxied :: Storable a => proxy a -> Int
sizeOfProxied = proxied sizeOf

-- | @'alignmentProxied' = 'proxied' 'alignment'@
--
-- /Since: 0.2/
alignmentProxied :: Storable a => proxy a -> Int
alignmentProxied = proxied alignment

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

#define T_TYPE(t) (t :: k -> (* -> *) -> * -> *)

-- | @'datatypeNameProxied' = 'proxied' 'datatypeName'@
--
-- /Since: 0.2/
datatypeNameProxied :: Datatype d
                    => proxy (T_TYPE(t) d f a)
                    -> [Char]
datatypeNameProxied = proxied datatypeName

-- | @'moduleNameProxied' = 'proxied' 'moduleName'@
--
-- /Since: 0.2/
moduleNameProxied :: Datatype d
                  => proxy (T_TYPE(t) d f a)
                  -> [Char]
moduleNameProxied = proxied moduleName

-- | @'isNewtypeProxied' = 'proxied' 'isNewtype'@
--
-- /Since: 0.2/
isNewtypeProxied :: Datatype d
                 => proxy (T_TYPE(t) d f a)
                 -> Bool
isNewtypeProxied = proxied isNewtype

-- | @'packageNameProxied' = 'proxied' 'packageName'@
--
-- /Since: 0.2/
packageNameProxied :: Datatype d
                   => proxy (T_TYPE(t) d f a)
                   -> [Char]
packageNameProxied = proxied packageName

-- | @'conNameProxied' = 'proxied' 'conName'@
--
-- /Since: 0.2/
conNameProxied :: Constructor c
               => proxy (T_TYPE(t) c f a)
               -> [Char]
conNameProxied = proxied conName

-- | @'conFixityProxied' = 'proxied' 'conFixity'@
--
-- /Since: 0.2/
conFixityProxied :: Constructor c
                 => proxy (T_TYPE(t) c f a)
                 -> Fixity
conFixityProxied = proxied conFixity

-- | @'conIsRecordProxied' = 'proxied' 'conIsRecord'@
--
-- /Since: 0.2/
conIsRecordProxied :: Constructor c
                   => proxy (T_TYPE(t) c f a)
                   -> Bool
conIsRecordProxied = proxied conIsRecord

-- | @'selNameProxied' = 'proxied' 'selName'@
--
-- /Since: 0.2/
selNameProxied :: Selector s
               => proxy (T_TYPE(t) s f a)
               -> [Char]
selNameProxied = proxied selName

-- | @'selSourceUnpackednessProxied' = 'proxied' 'selSourceUnpackedness'@
--
-- /Since: 0.2/
selSourceUnpackednessProxied :: Selector s
                             => proxy (T_TYPE(t) s f a)
                             -> SourceUnpackedness
selSourceUnpackednessProxied = proxied selSourceUnpackedness

-- | @'selSourceStrictnessProxied' = 'proxied' 'selSourceStrictness'@
--
-- /Since: 0.2/
selSourceStrictnessProxied :: Selector s
                           => proxy (T_TYPE(t) s f a)
                           -> SourceStrictness
selSourceStrictnessProxied = proxied selSourceStrictness

-- | @'selDecidedStrictnessProxied' = 'proxied' 'selDecidedStrictness'@
--
-- /Since: 0.2/
selDecidedStrictnessProxied :: Selector s
                            => proxy (T_TYPE(t) s f a)
                            -> DecidedStrictness
selDecidedStrictnessProxied = proxied selDecidedStrictness

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | @'floatRadixProxied' = 'proxied' 'floatRadix'@
--
-- /Since: 0.2/
floatRadixProxied :: RealFloat a => proxy a -> Integer
floatRadixProxied = proxied floatRadix

-- | @'floatDigitsProxied' = 'proxied' 'floatDigits'@
--
-- /Since: 0.2/
floatDigitsProxied :: RealFloat a => proxy a -> Int
floatDigitsProxied = proxied floatDigits

-- | @'floatRangeProxied' = 'proxied' 'floatRange'@
--
-- /Since: 0.2/
floatRangeProxied :: RealFloat a => proxy a -> (Int, Int)
floatRangeProxied = proxied floatRange

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

-- | @'parseFormatProxied' = 'proxied' 'parseFormat'@
--
-- /Since: 0.2/
parseFormatProxied :: PrintfArg a => proxy a -> ModifierParser
parseFormatProxied = proxied parseFormat
-}
