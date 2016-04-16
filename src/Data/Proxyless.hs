{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-type-defaults #-} -- Needed due to GHC Trac #11947

{-|
Module:      Data.Proxyless
Copyright:   (C) 2016 Ryan Scott
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
import Data.Data hiding (Fixity)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:))
import Data.Typeable.Internal (Typeable(..), typeNatTypeRep, typeSymbolTypeRep)

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
-- /Since: 0.2/
theTypeNatTypeRep :: forall a. KnownNat a => TypeRep
theTypeNatTypeRep = proxyHashless @a typeNatTypeRep

-- | @'theTypeRep' = 'proxyless' 'typeRep'@
--
-- /Since: 0.2/
theTypeRep :: forall a. Typeable a => TypeRep
theTypeRep = proxyless @a typeRep

-- | @'theTypeRep#' = 'proxyHashless' 'typeRep#'@
--
-- /Since: 0.2/
theTypeRep# :: forall a. Typeable a => TypeRep
theTypeRep# = proxyHashless @a typeRep#

-- | @'theTypeSymbolTypeRep' = 'proxyHashless' 'typeSymbolTypeRep'@
--
-- /Since: 0.2/
theTypeSymbolTypeRep :: forall a. KnownSymbol a => TypeRep
theTypeSymbolTypeRep = proxyHashless @a typeSymbolTypeRep

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

-- | @'theFromLabel' = 'proxyHashless' 'fromLabel'@
--
-- /Since: 0.2/
theFromLabel :: forall x a. IsLabel x a => a
theFromLabel = proxyHashless @x fromLabel

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
