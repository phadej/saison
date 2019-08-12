-- | Token definitions.
module Saison.Decoding.Tokens where

import Data.Scientific (Scientific)
import Data.Text       (Text)

-- | Tokens.
--
-- Note: 'Lit' exists to make 'Tokens' have only 6 constructors.
-- This may or may not have impact on performance.
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
