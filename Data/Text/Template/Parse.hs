{-# LANGUAGE NoImplicitPrelude #-}

-- | 'Data.Text.Template.Internal.Template' parametrised parsing API with
-- better error handling.
module Data.Text.Template.Parse
    ( ParseOptions(..)
    , parse

    -- * Parse Error
    , ParseError(..)
    , showTemplateError
    , showParseError
    , mkParseError
    , parseError
    )
  where

import Data.Text.Template.Internal
    ( ParseOptions(..)
    , ParseError(..)
    , showTemplateError
    , showParseError
    , mkParseError
    , parseError
    , parse
    )
