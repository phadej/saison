-- | Token definitions.
module Saison.Decoding.Tokens (
    -- * Types
    Tokens (..),
    Lit (..),
    TkArray (..),
    TkRecord (..),
    -- * Extras
    AsError (..),
    ) where

import Data.Scientific (Scientific)
import Data.Text       (Text)

-- | Tokens.
--
-- Note: 'Lit' exists to make 'Tokens' have only 6 constructors.
-- This may or may not have impact on performance.
--
-- * TODO: should number (and text) be represented by raw 'ByteString',
--   instead of 'Scientific' (and 'Text')?
--   With some guarantees that those are well-formed, so conversion
--   to 'Scientific' and 'Text' will succeed.
data Tokens k e
    = TkLit !Lit k
    | TkText !Text k
    | TkNumber !Scientific k
    | TkArrayOpen (TkArray k e)
    | TkRecordOpen (TkRecord k e)
    | TkErr e
  deriving (Eq, Show)

-- | Literals. @null@, @true@, @false@.
data Lit = LitNull | LitTrue | LitFalse
  deriving (Eq, Show)

-- | Array tokens.
data TkArray k e
    = TkItem (Tokens (TkArray k e) e)
    | TkArrayEnd k
    | TkArrayErr e
  deriving (Eq, Show)

-- | Record tokens.
data TkRecord k e
    = TkPair !Text (Tokens (TkRecord k e) e)
    | TkRecordEnd k
    | TkRecordErr e
  deriving (Eq, Show)

-- | Helper type-class for constructing errors.
class    AsError t        where tkErr :: e -> t k e
instance AsError Tokens   where tkErr = TkErr
instance AsError TkArray  where tkErr = TkArrayErr
instance AsError TkRecord where tkErr = TkRecordErr
