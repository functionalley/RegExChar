{-# LANGUAGE FlexibleContexts #-}
{-
	Copyright (C) 2010-2015 Dr. Alistair Ward

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Reads lines from the specified file.

	* Each line describes a 'Test'.

	* Each input-data string should match the /regex/ in the specified way.

	* Permits comparison with "Text.Regex.Posix.String".
-}

module Grecce.Test.Assert.RegExOptsChar(
-- * Types
-- ** Type-synonyms
--	Test,
--	TestDataAndResult,
--	TestFilter,
-- * Functions
	findFailures,
	findFailuresPosix,
	readTests
-- ** Predicates
--	isMatch
) where

import qualified	Control.Arrow
import qualified	RegExChar.ExtendedRegExChar	as ExtendedRegExChar
import qualified	RegExChar.RegExOptsChar		as RegExOptsChar	-- CAVEAT: beware of the similar name.
import qualified	RegExDot.CompilationOptions
import qualified	RegExDot.ExecutionOptions
import qualified	Text.Regex.Base.RegexLike	as RegexLike
import qualified	Text.Regex.Posix.Wrap
import			Text.Regex.Posix()

-- | Composed input data & list of captured 'RegExOptsChar.MatchSpan'.
type TestDataAndResult	= (ExtendedRegExChar.InputData, [RegExOptsChar.MatchSpan])

-- | Composed of the 'String' from which an unspecified type of regex is read, & a list of input data & results, to which it should respectively be applied & compared.
type Test		= (String {-regex-source-}, [TestDataAndResult])

-- | Filters a list of 'Test's, & returns a sub-set.
type TestFilter		= [Test] -> [Test]

-- | True if the specified 'ExtendedRegExChar.InputData', is a member of the set defined by the polymorphic 'Text.Regex.Base.RegexLike.RegexLike' parameter.
isMatch :: RegexLike.RegexLike r ExtendedRegExChar.InputData => r -> TestDataAndResult -> Bool
isMatch regex (inputData, matchSpanList)	= RegexLike.matchOnce regex inputData == Just (RegExOptsChar.toZeroIndexedArray matchSpanList)

-- | Returns those 'Test's, which 'RegExOptsChar.RegExOptsChar' fails.
findFailures :: RegExDot.CompilationOptions.CompilationOptions -> RegExDot.ExecutionOptions.ExecutionOptions -> TestFilter
findFailures compilationOptions executionOptions	= filter (not . uncurry all . Control.Arrow.first (isMatch . RegexLike.makeRegexOpts compilationOptions executionOptions))

-- | Returns those 'Test's, which 'Text.Regex.Posix.Wrap.Regex' fails.
findFailuresPosix :: TestFilter
findFailuresPosix	= filter (not . uncurry all . Control.Arrow.first ((isMatch :: Text.Regex.Posix.Wrap.Regex -> TestDataAndResult -> Bool) . RegexLike.makeRegex))

{- |
	* Read 'Test's from file, ignoring blank lines & comments.

	* To facilitate parsing, each line in the file has the format of @show Test@.
-}
readTests :: String -> IO [Test]
readTests fileName	= (
	map (
		\line	-> case reads line of
			[(test, _)]	-> test
			_		-> error $ "Grecce.Test.Assert.RegExOptsChar.readTests:\tfailed to parse " ++ show fileName ++ "; " ++ show line ++ "."
	) . filter (
		(/= '#') . head	-- Remove comments.
	) . filter (
		not . null	-- Remove blank lines.
	) . lines
 ) `fmap` readFile fileName

