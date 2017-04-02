{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

#if __GLASGOW_HASKELL__ == 800 \
    && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1
{-# OPTIONS_GHC -Wno-type-defaults #-} -- Needed due to GHC Trac #11947
#endif

{-|
Module:      Data.Proxyless
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Remove the 'Proxy', 'Proxy#', and 'undefined' arguments from functions with
'proxyless', 'proxyHashless', and 'undefinedless', respectively, which produce
functions that take type information via GHC's @-XTypeApplications@ extension.

This module is only available with GHC 8.0 or later.

/Since: 0.2/
-}
module Data.Proxyless (
      -- * 'proxyless', 'proxyHashless', and 'undefinedless'
      proxyless
    , proxyHashless
    , undefinedless
      -- * Proxyless functions
      -- ** "Data.Bits"
    , theBitSize
    , theIsSigned
    , theBitSizeMaybe
    , theFiniteBitSize
      -- ** "Data.Data"
    , theDataTypeOf
      -- ** "Data.Typeable"
    , theTypeNatTypeRep
    , theTypeRep
    , theTypeRep#
    , theTypeSymbolTypeRep
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
      -- ** "GHC.OverloadedLabels"
    , theFromLabel
      -- ** "GHC.TypeLits"
    , theNatVal
    , theNatVal'
    , theSameNat
    , theSameSymbol
    , theSomeNat
    , theSomeSymbol
    , theSymbolVal
    , theSymbolVal'
      -- ** "Prelude"
    , theFloatRadix
    , theFloatDigits
    , theFloatRange
      -- ** "Text.Printf"
    , theParseFormat
    ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Data (Data(dataTypeOf), DataType)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:))
import Data.Typeable (Typeable, TypeRep, typeRep)
#if !(MIN_VERSION_base(4,10,0))
import Data.Typeable.Internal (typeRep#, typeNatTypeRep, typeSymbolTypeRep)
#endif

import Foreign.Storable (Storable(..))

import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits

import Text.Printf (PrintfArg(..), ModifierParser)

-- | Converts a constant function that takes a 'Proxy' argument to one that
-- doesn't require an argument.
--
-- /Since: 0.2/
proxyless :: forall a b. (Proxy a -> b) -> b
proxyless f = f Proxy

-- | Converts a constant function that takes a 'Proxy#' argument to one that
-- doesn't require an argument.
--
-- /Since: 0.2/
proxyHashless :: forall a b. (Proxy# a -> b) -> b
proxyHashless f = f proxy#

-- | Converts a constant function that takes an 'undefined' argument to one that
-- doesn't require an argument.
--
-- /Since: 0.2/
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

-------------------------------------------------------------------------------
-- Data.Data
-------------------------------------------------------------------------------

-- | @'theDataTypeOf' = 'undefinedless' 'dataTypeOf'@
--
-- /Since: 0.2/
theDataTypeOf :: forall a. Data a => DataType
theDataTypeOf = undefinedless @a dataTypeOf

-------------------------------------------------------------------------------
-- Data.Typeable
-------------------------------------------------------------------------------

-- | @'theTypeNatTypeRep' = 'proxyHashless' 'typeNatTypeRep'@
--
-- Note that in @base-4.10@ and later, 'theTypeNatTypeRep' is simply a synonym
-- for @'theTypeRep' \@'Nat'@, as 'typeNatTypeRep' is no longer exported.
--
-- /Since: 0.2/
theTypeNatTypeRep :: forall a. KnownNat a => TypeRep
#if MIN_VERSION_base(4,10,0)
theTypeNatTypeRep = theTypeRep @Nat
#else
theTypeNatTypeRep = proxyHashless @a typeNatTypeRep
#endif

-- | @'theTypeRep' = 'proxyless' 'typeRep'@
--
-- /Since: 0.2/
theTypeRep :: forall a. Typeable a => TypeRep
theTypeRep = proxyless @a typeRep

-- | @'theTypeRep#' = 'proxyHashless' 'typeRep#'@
--
-- Note that in @base-4.10@ and later, 'theTypeRep#' is simply a synonym for
-- 'theTypeRep', as 'typeRep#' is no longer exported.
--
-- /Since: 0.2/
theTypeRep# :: forall a. Typeable a => TypeRep
#if MIN_VERSION_base(4,10,0)
theTypeRep# = theTypeRep @a
#else
theTypeRep# = proxyHashless @a typeRep#
#endif

-- | @'theTypeSymbolTypeRep' = 'proxyHashless' 'typeSymbolTypeRep'@
--
-- Note that in @base-4.10@ and later, 'theTypeSymbolTypeRep' is simply a
-- synonym for @'theTypeRep' \@'Symbol'@, as 'typeSymbolTypeRep' is no longer
-- exported.
--
-- /Since: 0.2/
theTypeSymbolTypeRep :: forall a. KnownSymbol a => TypeRep
#if MIN_VERSION_base(4,10,0)
theTypeSymbolTypeRep = theTypeRep @Symbol
#else
theTypeSymbolTypeRep = proxyHashless @a typeSymbolTypeRep
#endif

-------------------------------------------------------------------------------
-- Foreign.Storable
-------------------------------------------------------------------------------

-- | @'theSizeOf' = 'undefinedless' 'sizeOf'@
--
-- /Since: 0.2/
theSizeOf :: forall a. Storable a => Int
theSizeOf = undefinedless @a sizeOf

-- | @'theAlignment' = 'undefinedless' 'alignment'@
--
-- /Since: 0.2/
theAlignment :: forall a. Storable a => Int
theAlignment = undefinedless @a alignment

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

-- | @'theDatatypeName' = 'datatypeName' 'undefined'@
--
-- /Since: 0.2/
theDatatypeName :: forall d. Datatype d => [Char]
theDatatypeName = datatypeName @d undefined

-- | @'theModuleName' = 'moduleName' 'undefined'@
--
-- /Since: 0.2/
theModuleName :: forall d. Datatype d => [Char]
theModuleName = moduleName @d undefined

-- | @'theIsNewtype' = 'isNewtype' 'undefined'@
--
-- /Since: 0.2/
theIsNewtype :: forall d. Datatype d => Bool
theIsNewtype = isNewtype @d undefined

-- | @'thePackageName' = 'packageName' 'undefined'@
--
-- /Since: 0.2/
thePackageName :: forall d. Datatype d => [Char]
thePackageName = packageName @d undefined

-- | @'theConName' = 'conName' 'undefined'@
--
-- /Since: 0.2/
theConName :: forall c. Constructor c => [Char]
theConName = conName @c undefined

-- | @'theConFixity' = 'conFixity' 'undefined'@
--
-- /Since: 0.2/
theConFixity :: forall c. Constructor c => Fixity
theConFixity = conFixity @c undefined

-- | @'theConIsRecord' = 'conIsRecord' 'undefined'@
--
-- /Since: 0.2/
theConIsRecord :: forall c. Constructor c => Bool
theConIsRecord = conIsRecord @c undefined

-- | @'theSelName' = 'selName' 'undefined'@
--
-- /Since: 0.2/
theSelName :: forall s. Selector s => [Char]
theSelName = selName @s undefined

-- | @'theSelSourceUnpackedness' = 'selSourceUnpackedness' 'undefined'@
--
-- /Since: 0.2/
theSelSourceUnpackedness :: forall s. Selector s => SourceUnpackedness
theSelSourceUnpackedness = selSourceUnpackedness @s undefined

-- | @'theSelSourceStrictness' = 'selSourceStrictness' 'undefined'@
--
-- /Since: 0.2/
theSelSourceStrictness :: forall s. Selector s => SourceStrictness
theSelSourceStrictness = selSourceStrictness @s undefined

-- | @'theSelDecidedStrictness' = 'selDecidedStrictness' 'undefined'@
--
-- /Since: 0.2/
theSelDecidedStrictness :: forall s. Selector s => DecidedStrictness
theSelDecidedStrictness = selDecidedStrictness @s undefined

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

-- | In @base-4.10@ and later, this is simply a synonym for 'fromLabel'.
-- In @base-4.9@, 'theFromLabel' is defined as:
--
-- @'theFromLabel' = 'proxyHashless' 'fromLabel'@
--
-- /Since: 0.2/
theFromLabel :: forall x a. IsLabel x a => a
#if MIN_VERSION_base(4,10,0)
theFromLabel = fromLabel @x
#else
theFromLabel = proxyHashless @x fromLabel
#endif

-------------------------------------------------------------------------------
-- GHC.TypeLits
-------------------------------------------------------------------------------

-- | @'theNatVal' = 'proxyless' 'natVal'@
--
-- /Since: 0.2/
theNatVal :: forall n. KnownNat n => Integer
theNatVal = proxyless @n natVal

-- | @`theNatVal'` = 'proxyHashless' `natVal'`@
--
-- /Since: 0.2/
theNatVal' :: forall n. KnownNat n => Integer
theNatVal' = proxyHashless @n natVal'

-- | @'theSameNat' = 'sameNat' 'Proxy' 'Proxy'@
--
-- /Since: 0.2/
theSameNat :: forall a b. (KnownNat a, KnownNat b) => Maybe (a :~: b)
theSameNat = sameNat (Proxy @a) (Proxy @b)

-- | @'theSameSymbol' = 'sameSymbol' 'Proxy' 'Proxy'@
--
-- /Since: 0.2/
theSameSymbol :: forall a b. (KnownSymbol a, KnownSymbol b) => Maybe (a :~: b)
theSameSymbol = sameSymbol (Proxy @a) (Proxy @b)

-- | @'theSomeNat' = 'proxyless' 'SomeNat'@
--
-- /Since: 0.2/
theSomeNat :: forall n. KnownNat n => SomeNat
theSomeNat = proxyless @n SomeNat

-- | @'theSomeSymbol' = 'proxyless' 'SomeSymbol'@
--
-- /Since: 0.2/
theSomeSymbol :: forall n. KnownSymbol n => SomeSymbol
theSomeSymbol = proxyless @n SomeSymbol

-- | @'theSymbolVal' = 'proxyless' 'symbolVal'@
--
-- /Since: 0.2/
theSymbolVal :: forall n. KnownSymbol n => String
theSymbolVal = proxyless @n symbolVal

-- | @`theSymbolVal'` = 'proxyHashless' `symbolVal'`@
--
-- /Since: 0.2/
theSymbolVal' :: forall n. KnownSymbol n => String
theSymbolVal' = proxyHashless @n symbolVal'

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | @'theFloatRadix' = 'undefinedless' 'floatRadix'@
--
-- /Since: 0.2/
theFloatRadix :: forall a. RealFloat a => Integer
theFloatRadix = undefinedless @a floatRadix

-- | @'theFloatDigits' = 'undefinedless' 'floatDigits'@
--
-- /Since: 0.2/
theFloatDigits :: forall a. RealFloat a => Int
theFloatDigits = undefinedless @a floatDigits

-- | @'theFloatRange' = 'undefinedless' 'floatRange'@
--
-- /Since: 0.2/
theFloatRange :: forall a. RealFloat a => (Int, Int)
theFloatRange = undefinedless @a floatRange

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

-- | @'theParseFormat' = 'undefinedless' 'parseFormat'@
--
-- /Since: 0.2/
theParseFormat :: forall a. PrintfArg a => ModifierParser
theParseFormat = undefinedless @a parseFormat
