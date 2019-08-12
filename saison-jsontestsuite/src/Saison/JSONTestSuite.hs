module Saison.JSONTestSuite (jsonTestSuite) where

import Data.Aeson       (Value)
import Data.ByteString  (ByteString)
import Data.Either      (isLeft, isRight)
import Data.List        (sort)
import Data.Set         (Set)
import Data.Traversable (for)
import System.Directory (getDirectoryContents)
import System.FilePath  (takeExtension, takeFileName, (</>))
import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import qualified Data.ByteString as BS
import qualified Data.Set        as Set

jsonTestSuite :: (ByteString -> Either String Value) -> IO ()
jsonTestSuite decode = do
    tree <- jsonTestSuiteTree decode
    defaultMain tree

-- | Build a collection of tests based on the current contents of the
-- JSONTestSuite test directories.
jsonTestSuiteTree :: (ByteString -> Either String Value)  -> IO TestTree
jsonTestSuiteTree decode = do
    let suitePath = "JSONTestSuite"
    let suites = ["test_parsing", "test_transform"]
    testPaths <- fmap (sort . concat) $ for suites $ \suite -> do
      let dir = suitePath </> suite
      entries <- getDirectoryContents dir
      return
          [ (name, dir </> name)
          | name <- entries
          , takeExtension name ==  ".json"
          ]

    let blacklistTest = testCase "Blacklist contains no extras" $ do
            let names = Set.fromList $ map fst testPaths
            let extras = blacklist `Set.difference` names
            if Set.null extras
            then return ()
            else assertFailure $ "contains extras " ++ show (Set.toList extras)

    return $ testGroup "JSONTestSuite" $
        blacklistTest :
        [ jsonTestSuiteTest decode blacklisted path
        | (name, path) <- testPaths
        , let blacklisted = Set.member name blacklist
        ]

jsonTestSuiteTest
    :: (ByteString -> Either String Value)
    -> Bool
    -> FilePath
    -> TestTree
jsonTestSuiteTest decode blacklisted path = testCase fileName $ do
    contents <- BS.readFile path
    expect (decode contents)
  where
    fileName = takeFileName path

    expect = expect' $ invert $ case take 2 fileName of
        "i_" -> True
        "n_" -> False
        "y_" -> True
        _    -> True -- test_transform tests have inconsistent names

    -- blacklisting inverts expectation
    invert | blacklisted = not
           | otherwise   = id

    expect' True  = expectSuccess
    expect' False = expectFailure

    expectSuccess (Right _)  = return ()
    expectSuccess (Left err) = assertFailure err

    expectFailure (Right _) = assertFailure "Unexpected success"
    expectFailure (Left _)  = return ()

-------------------------------------------------------------------------------
-- Blacklist
-------------------------------------------------------------------------------

-- The set expected-to-be-failing JSONTestSuite tests.
-- Not all of these failures are genuine bugs.
-- Of those that are bugs, not all are worth fixing.
--
-- 2019-08-12 this is the same list as in aeson-1.4.4.0

blacklist :: Set String
blacklist = _blacklist
-- blacklist = Set.empty

-- | test_parsing files are named according to these rules:
--
-- * @y_@ content must be accepted by parsers
-- * @n_@ content must be rejected by parsers
-- * @i_@ parsers are free to accept or reject content
--
-- In general, we try to accept @_i@ files. But some we might as well
-- reject.
--
-- There are three accepted @_n@ tests: @aeson@ (and thus @saison@) accept
-- unescaped control characters newlines, and tabs.
--
_blacklist :: Set String
_blacklist = Set.fromList
    [ "i_object_key_lone_2nd_surrogate.json"
    , "i_string_1st_surrogate_but_2nd_missing.json"
    , "i_string_1st_valid_surrogate_2nd_invalid.json"
    , "i_string_incomplete_surrogate_and_escape_valid.json"
    , "i_string_incomplete_surrogate_pair.json"
    , "i_string_incomplete_surrogates_escape_valid.json"
    , "i_string_invalid_lonely_surrogate.json"
    , "i_string_invalid_surrogate.json"
    , "i_string_invalid_utf-8.json"
    , "i_string_inverted_surrogates_U+1D11E.json"
    , "i_string_iso_latin_1.json"
    , "i_string_lone_second_surrogate.json"
    , "i_string_lone_utf8_continuation_byte.json"
    , "i_string_not_in_unicode_range.json"
    , "i_string_overlong_sequence_2_bytes.json"
    , "i_string_overlong_sequence_6_bytes.json"
    , "i_string_overlong_sequence_6_bytes_null.json"
    , "i_string_truncated-utf-8.json"
    , "i_string_UTF-16LE_with_BOM.json"
    , "i_string_UTF-8_invalid_sequence.json"
    , "i_string_utf16BE_no_BOM.json"
    , "i_string_utf16LE_no_BOM.json"
    , "i_string_UTF8_surrogate_U+D800.json"
    , "i_structure_UTF-8_BOM_empty_object.json"
    , "n_string_unescaped_crtl_char.json"
    , "n_string_unescaped_newline.json"
    , "n_string_unescaped_tab.json"
    , "string_1_escaped_invalid_codepoint.json"
    , "string_1_invalid_codepoint.json"
    , "string_2_escaped_invalid_codepoints.json"
    , "string_2_invalid_codepoints.json"
    , "string_3_escaped_invalid_codepoints.json"
    , "string_3_invalid_codepoints.json"
    ]
