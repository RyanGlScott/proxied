{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-|
Module:      Data.Proxied
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Convert 'undefined'-consuming functions to 'Proxy'-consuming ones with 'proxied'.

/Since: 0.1/
-}
module Data.Proxied (
      -- * 'proxied' and 'unproxied'
      proxied
#if MIN_VERSION_base(4,7,0)
    , proxyHashed
#endif
    , unproxied
    , module Data.Proxy
      -- * Proxified functions
      -- ** "Data.Bits"
    , bitSizeProxied
    , isSignedProxied
#if MIN_VERSION_base(4,7,0)
    , bitSizeMaybeProxied
    , finiteBitSizeProxied
#endif
      -- ** "Data.Data"
    , dataTypeOfProxied
      -- ** "Data.Typeable"
    , typeOfProxied
      -- ** "Foreign.Storable"
    , sizeOfProxied
    , alignmentProxied
      -- ** "GHC.Generics"
    , datatypeNameProxied
    , moduleNameProxied
#if MIN_VERSION_base(4,7,0)
    , isNewtypeProxied
#endif
#if MIN_VERSION_base(4,9,0)
    , packageNameProxied
#endif
    , conNameProxied
    , conFixityProxied
    , conIsRecordProxied
    , selNameProxied
#if MIN_VERSION_base(4,9,0)
    , selSourceUnpackednessProxied
    , selSourceStrictnessProxied
    , selDecidedStrictnessProxied
#endif
      -- ** "Prelude"
    , floatRadixProxied
    , floatDigitsProxied
    , floatRangeProxied
#if MIN_VERSION_base(4,7,0)
      -- ** "Text.Printf"
    , parseFormatProxied
#endif
    ) where

import Data.Bits (Bits(..))
import Data.Data hiding (Fixity)
import Data.Proxy

import Foreign.Storable (Storable(..))

#if MIN_VERSION_base(4,6,0)
import GHC.Generics
#else
import Generics.Deriving.Base
import Generics.Deriving.Instances ()
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(..))
import GHC.Exts (Proxy#)
import Text.Printf (PrintfArg(..), ModifierParser)
#endif

-- | Converts a constant function to one that takes a @proxy@ argument.
--
-- /Since: 0.1/
proxied :: (a -> b) -> proxy a -> b
proxied f _ = f undefined

#if MIN_VERSION_base(4,7,0)
-- | Converts a constant function to one that takes a @Proxy#@ argument.
-- This function is only available with @base-4.7@ or later.
--
-- /Since: 0.2/
proxyHashed :: (a -> b) -> Proxy# a -> b
proxyHashed f _ = f undefined
#endif

-- | Converts a constant function that takes a 'Proxy' argument to one that
-- doesn't require a @proxy@ argument. (I'm not sure why you'd want this,
-- but it's here for symmetry.)
--
-- /Since: 0.1/
unproxied :: (Proxy a -> b) -> a -> b
unproxied f _ = f Proxy

-------------------------------------------------------------------------------
-- Data.Bits
-------------------------------------------------------------------------------

-- | @'bitSizeProxied' = 'proxied' 'bitSize'@
--
-- /Since: 0.1/
bitSizeProxied :: Bits a => proxy a -> Int
bitSizeProxied = proxied bitSize

-- | @'isSignedProxied' = 'proxied' 'isSigned'@
--
-- /Since: 0.1/
isSignedProxied :: Bits a => proxy a -> Bool
isSignedProxied = proxied isSigned

#if MIN_VERSION_base(4,7,0)
-- | @'bitSizeMaybeProxied' = 'proxied' 'bitSizeMaybe'@
--
-- This function is only available with @base-4.7@ or later.
--
-- /Since: 0.1/
bitSizeMaybeProxied :: Bits a => proxy a -> Maybe Int
bitSizeMaybeProxied = proxied bitSizeMaybe

-- | @'finiteBitSizeProxied' = 'proxied' 'finiteBitSize'@
--
-- This function is only available with @base-4.7@ or later.
--
-- /Since: 0.1/
finiteBitSizeProxied :: FiniteBits a => proxy a -> Int
finiteBitSizeProxied = proxied finiteBitSize
#endif

-------------------------------------------------------------------------------
-- Data.Data
-------------------------------------------------------------------------------

-- | @'dataTypeOfProxied' = 'proxied' 'dataTypeOf'@
--
-- /Since: 0.1/
dataTypeOfProxied :: Data a => proxy a -> DataType
dataTypeOfProxied = proxied dataTypeOf

-------------------------------------------------------------------------------
-- Data.Typeable
-------------------------------------------------------------------------------

-- | @'typeOfProxied' = 'proxied' 'typeOf'@
--
-- On @base-4.7@ and later, this is identical to 'typeRep'.
--
-- /Since: 0.1/
typeOfProxied :: Typeable a => proxy a -> TypeRep
#if MIN_VERSION_base(4,7,0)
typeOfProxied = typeRep
#else
typeOfProxied = proxied typeOf
#endif

-------------------------------------------------------------------------------
-- Foreign.Storable
-------------------------------------------------------------------------------

-- | @'sizeOfProxied' = 'proxied' 'sizeOf'@
--
-- /Since: 0.1/
sizeOfProxied :: Storable a => proxy a -> Int
sizeOfProxied = proxied sizeOf

-- | @'alignmentProxied' = 'proxied' 'alignment'@
--
-- /Since: 0.1/
alignmentProxied :: Storable a => proxy a -> Int
alignmentProxied = proxied alignment

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,9,0)
# define T_TYPE(t) (t :: k -> (* -> *) -> * -> *)
#else
# define T_TYPE(t) (t :: * -> (* -> *) -> * -> *)
#endif

-- | @'datatypeNameProxied' = 'proxied' 'datatypeName'@
--
-- /Since: 0.1/
datatypeNameProxied :: Datatype d
                    => proxy (T_TYPE(t) d f a)
                    -> [Char]
datatypeNameProxied = proxied datatypeName

-- | @'moduleNameProxied' = 'proxied' 'moduleName'@
--
-- /Since: 0.1/
moduleNameProxied :: Datatype d
                  => proxy (T_TYPE(t) d f a)
                  -> [Char]
moduleNameProxied = proxied moduleName

#if MIN_VERSION_base(4,7,0)
-- | @'isNewtypeProxied' = 'proxied' 'isNewtype'@
--
-- This function is only available with @base-4.7@ or later.
--
-- /Since: 0.1/
isNewtypeProxied :: Datatype d
                 => proxy (T_TYPE(t) d f a)
                 -> Bool
isNewtypeProxied = proxied isNewtype
#endif

#if MIN_VERSION_base(4,9,0)
-- | @'packageNameProxied' = 'proxied' 'packageName'@
--
-- This function is only avaiable with @base-4.9@ or later.
--
-- /Since: 0.1/
packageNameProxied :: Datatype d
                   => proxy (T_TYPE(t) d f a)
                   -> [Char]
packageNameProxied = proxied packageName
#endif

-- | @'conNameProxied' = 'proxied' 'conName'@
--
-- /Since: 0.1/
conNameProxied :: Constructor c
               => proxy (T_TYPE(t) c f a)
               -> [Char]
conNameProxied = proxied conName

-- | @'conFixityProxied' = 'proxied' 'conFixity'@
--
-- /Since: 0.1/
conFixityProxied :: Constructor c
                 => proxy (T_TYPE(t) c f a)
                 -> Fixity
conFixityProxied = proxied conFixity

-- | @'conIsRecordProxied' = 'proxied' 'conIsRecord'@
--
-- /Since: 0.1/
conIsRecordProxied :: Constructor c
                   => proxy (T_TYPE(t) c f a)
                   -> Bool
conIsRecordProxied = proxied conIsRecord

-- | @'selNameProxied' = 'proxied' 'selName'@
--
-- /Since: 0.1/
selNameProxied :: Selector s
               => proxy (T_TYPE(t) s f a)
               -> [Char]
selNameProxied = proxied selName

#if MIN_VERSION_base(4,9,0)
-- | @'selSourceUnpackednessProxied' = 'proxied' 'selSourceUnpackedness'@
--
-- This function is only available with @base-4.9@ or later.
--
-- /Since: 0.1/
selSourceUnpackednessProxied :: Selector s
                             => proxy (T_TYPE(t) s f a)
                             -> SourceUnpackedness
selSourceUnpackednessProxied = proxied selSourceUnpackedness

-- | @'selSourceStrictnessProxied' = 'proxied' 'selSourceStrictness'@
--
-- This function is only available with @base-4.9@ or later.
--
-- /Since: 0.1/
selSourceStrictnessProxied :: Selector s
                           => proxy (T_TYPE(t) s f a)
                           -> SourceStrictness
selSourceStrictnessProxied = proxied selSourceStrictness

-- | @'selDecidedStrictnessProxied' = 'proxied' 'selDecidedStrictness'@
--
-- This function is only available with @base-4.9@ or later.
--
-- /Since: 0.1/
selDecidedStrictnessProxied :: Selector s
                            => proxy (T_TYPE(t) s f a)
                            -> DecidedStrictness
selDecidedStrictnessProxied = proxied selDecidedStrictness
#endif

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | @'floatRadixProxied' = 'proxied' 'floatRadix'@
--
-- /Since: 0.1/
floatRadixProxied :: RealFloat a => proxy a -> Integer
floatRadixProxied = proxied floatRadix

-- | @'floatDigitsProxied' = 'proxied' 'floatDigits'@
--
-- /Since: 0.1/
floatDigitsProxied :: RealFloat a => proxy a -> Int
floatDigitsProxied = proxied floatDigits

-- | @'floatRangeProxied' = 'proxied' 'floatRange'@
--
-- /Since: 0.1/
floatRangeProxied :: RealFloat a => proxy a -> (Int, Int)
floatRangeProxied = proxied floatRange

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,7,0)
-- | @'parseFormatProxied' = 'proxied' 'parseFormat'@
--
-- This function is only available with @base-4.7@ or later.
--
-- /Since: 0.1/
parseFormatProxied :: PrintfArg a => proxy a -> ModifierParser
parseFormatProxied = proxied parseFormat
#endif
