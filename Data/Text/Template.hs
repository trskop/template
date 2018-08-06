{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple string substitution library that supports \"$\"-based
-- substitution. Substitution uses the following rules:
--
--    * \"$$\" is an escape; it is replaced with a single \"$\".
--
--    * \"$identifier\" names a substitution placeholder matching a
--      mapping key of \"identifier\". \"identifier\" must spell a
--      Haskell identifier. The first non-identifier character after the
--      \"$\" character terminates this placeholder specification.
--
--    * \"${identifier}\" is equivalent to \"$identifier\". It is
--      required when valid identifier characters follow the placeholder
--      but are not part of the placeholder, such as
--      \"${noun}ification\".
--
-- Any other appearance of \"$\" in the string will result in an
-- 'Prelude.error' being raised.
--
-- If you render the same template multiple times it's faster to first
-- convert it to a more efficient representation using 'template' and
-- then render it using 'render'. In fact, all that 'substitute' does
-- is to combine these two steps.

module Data.Text.Template
    (
     -- * The @Template@ type
     Template,

     -- * The @Context@ type
     Context,
     ContextA,

     -- * Basic interface
     template,
     templateSafe,
     render,
     substitute,
     showTemplate,

     -- * Applicative interface
     renderA,
     substituteA,

     -- * Example
     -- $example
    ) where

import Data.Text.Template.Internal
    ( Context
    , ContextA
    , Template
    , render
    , renderA
    , showTemplate
    , substitute
    , substituteA
    , template
    , templateSafe
    )

-- $example
--
-- Here is an example of a simple substitution:
--
-- > module Main where
-- >
-- > import qualified Data.ByteString.Lazy as S
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Lazy.Encoding as E
-- >
-- > import Data.Text.Template
-- >
-- > -- | Create 'Context' from association list.
-- > context :: [(T.Text, T.Text)] -> Context
-- > context assocs x = maybe err id . lookup x $ assocs
-- >   where err = error $ "Could not find key: " ++ T.unpack x
-- >
-- > main :: IO ()
-- > main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
-- >   where
-- >     helloTemplate = T.pack "Hello, $name!\n"
-- >     helloContext  = context [(T.pack "name", T.pack "Joe")]
--
-- The example can be simplified slightly by using the
-- @OverloadedStrings@ language extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main where
-- >
-- > import qualified Data.ByteString.Lazy as S
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Lazy.Encoding as E
-- >
-- > import Data.Text.Template
-- >
-- > -- | Create 'Context' from association list.
-- > context :: [(T.Text, T.Text)] -> Context
-- > context assocs x = maybe err id . lookup x $ assocs
-- >   where err = error $ "Could not find key: " ++ T.unpack x
-- >
-- > main :: IO ()
-- > main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
-- >   where
-- >     helloTemplate = "Hello, $name!\n"
-- >     helloContext  = context [("name", "Joe")]
