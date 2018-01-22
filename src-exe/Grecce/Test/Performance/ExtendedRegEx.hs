{-# LANGUAGE CPP #-}
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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* CPU-times the specified stress-test(s), over a hard-coded list of /regex/-engines,
	providing each combination with a varied list of 'Performance.ExtendedRegExTest.TestGenerator's,
	& a level of 'Performance.ExtendedRegExTest.TestComplexity' which increases linearly to the specified cap.

	* Either a simple 'Bool' result, or the full data-capture, can be requested.

 [@CAVEATS@]

	* For compatibility with the other regex-engines, uses the specialised instance 'ExtendedRegExChar.ExtendedRegExChar',
	rather than the underlying polymorphic 'RegExDot.RegEx.ExtendedRegEx'.

	* "Text.Regex.Posix" stalls forever on 'Performance.ExtendedRegExTestsPositive.OVERLAPPING_ALTERNATIVES',
	with 'Performance.ExtendedRegExTest.TestComplexity'=3 & data-capture requested.

 [@TODO@]

	* Utilise the standard interface in "RegExOptsChar", to permit identical treatment of different regex-engines.
-}

module Grecce.Test.Performance.ExtendedRegEx(
-- * Types
-- ** Type-synonyms
--	GenerateAndRunTest,
--	GetAndRunTestRange,
-- * Constants
--	solitary,
-- * Functions
	run
--	tailorTest
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Grecce.Test.Performance.ExtendedRegExTest		as Test.Performance.ExtendedRegExTest
import qualified	Grecce.Test.Performance.ExtendedRegExTestsNegative	as Test.Performance.ExtendedRegExTestsNegative
import qualified	Grecce.Test.Performance.ExtendedRegExTestsPositive	as Test.Performance.ExtendedRegExTestsPositive
import qualified	RegExChar.ExtendedRegExChar				as ExtendedRegExChar
import qualified	RegExDot.ExecutionOptions
import qualified	RegExDot.RegEx
import qualified	RegExDot.RegExOpts
import qualified	Text.Regex.Posix
import qualified	ToolShed.System.TimePure

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

solitary :: Bool
solitary	= True	-- Currently required because of the disastrous performance of "Text.Regex.Posix" on 'Test.Performance.ExtendedRegExTestsPositive.OVERLAPPING_ALTERNATIVES'.

type GetAndRunTestRange a	= [a] -> IO ()
type GenerateAndRunTest		= Test.Performance.ExtendedRegExTest.TestGenerator Char -> Test.Performance.ExtendedRegExTest.TestComplexity -> IO (ExtendedRegExChar.InputData, [(Double, String)])

{- |
	* Print the CPU-seconds, required to run the range of 'Test.Performance.ExtendedRegExTest.TestGenerator's corresponding to the specified test-names & a linearly increasing 'Test.Performance.ExtendedRegExTest.TestComplexity'.

	* If the specified maximum 'Test.Performance.ExtendedRegExTest.TestComplexity' is set to the special value '0', then it is increased linearly, without limit.
-}
run :: RegExDot.ExecutionOptions.ExecutionOptions -> Test.Performance.ExtendedRegExTest.TestComplexity -> Either [Test.Performance.ExtendedRegExTestsNegative.TestName] [Test.Performance.ExtendedRegExTestsPositive.TestName] -> IO ()
run executionOptions 0 testRange					= run executionOptions maxBound testRange	-- Run the specified test-range, without any upper bound to 'Test.Performance.ExtendedRegExTest.TestComplexity'.
run executionOptions maxTestComplexity (Left negativeTestRange)		= runNegativeTestRange negativeTestRange	where
	runNegativeTestRange :: GetAndRunTestRange Test.Performance.ExtendedRegExTestsNegative.TestName
	runNegativeTestRange []		= runNegativeTestRange [minBound .. maxBound]	-- Run all the tests, to the specified maximum 'Test.Performance.ExtendedRegExTest.TestComplexity'.
	runNegativeTestRange testRange	= sequence_ $ (
		\complexity testGenerator	-> cpuSecondsFor testGenerator complexity >>= print
	 ) <$> [1 .. maxTestComplexity] <*> Data.Maybe.mapMaybe Test.Performance.ExtendedRegExTest.query testRange	where
		cpuSecondsFor :: GenerateAndRunTest
		cpuSecondsFor testGenerator	= (
			\(inputData, (isMismatch, extendedRegExCharString))	-> (,) extendedRegExCharString <$> mapM (
				ToolShed.System.TimePure.getCPUSeconds . ($ inputData) . (show .)
			 ) [
				isMismatch,
				if solitary
					then const True	-- CAVEAT: should be 'undefined'.
					else not . (Text.Regex.Posix.=~ extendedRegExCharString) :: String -> Bool	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
			]
		 ) . Control.Arrow.second (
			(
				\extendedRegEx -> (
					RegExDot.RegEx./~ RegExDot.RegExOpts.MkRegExOpts {	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
						RegExDot.RegExOpts.compilationOptions	= Data.Default.def,
						RegExDot.RegExOpts.executionOptions	= executionOptions,
						RegExDot.RegExOpts.regEx		= extendedRegEx
					}
				)
			) &&& show . ExtendedRegExChar.MkExtendedRegExChar False {-hasNonCapturingTopLevelAlternatives-}
		 ) . testGenerator

run executionOptions maxTestComplexity (Right positiveTestRange)	= runPositiveTestRange positiveTestRange	where
	runPositiveTestRange :: GetAndRunTestRange Test.Performance.ExtendedRegExTestsPositive.TestName
	runPositiveTestRange []		= runPositiveTestRange [minBound .. maxBound]	-- Run all the tests, to the specified maximum 'Test.Performance.ExtendedRegExTest.TestComplexity'.
	runPositiveTestRange testRange	= sequence_ $ (
		\complexity testGenerator	-> cpuSecondsFor testGenerator complexity >>= print
	 ) <$> [1 .. maxTestComplexity] <*> Data.Maybe.mapMaybe Test.Performance.ExtendedRegExTest.query testRange	where
		cpuSecondsFor :: GenerateAndRunTest
		cpuSecondsFor testGenerator	= (
			\(inputData, (showMatch, extendedRegExCharString))	-> (,) extendedRegExCharString <$> (
				(ToolShed.System.TimePure.getCPUSeconds . ($ inputData)) `mapM` [
					showMatch,
					if RegExDot.ExecutionOptions.requireMatchList executionOptions
						then if solitary
							then const "<untested>"
--							else (Text.Regex.Posix.=~ extendedRegExCharString) :: String -> String	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
							else (\(_, _, _, s) -> show s) . ((Text.Regex.Posix.=~ extendedRegExCharString) :: String -> (String, String, String, [String]))	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
						else show . if solitary
							then const True	-- CAVEAT: should be 'undefined'.
							else (Text.Regex.Posix.=~ extendedRegExCharString) :: String -> Bool	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
				]
			)
		 ) . Control.Arrow.second (
			(
				\extendedRegEx -> let
					regExOpts :: RegExDot.RegExOpts.RegExOpts (RegExDot.RegEx.ExtendedRegEx Char)
					regExOpts	= RegExDot.RegExOpts.MkRegExOpts {
						RegExDot.RegExOpts.compilationOptions	= Data.Default.def,
						RegExDot.RegExOpts.executionOptions	= executionOptions,
						RegExDot.RegExOpts.regEx		= extendedRegEx
					} -- Similar to 'RegexLike.makeRegexOpts', but without the requirement to read the regex.
				in if RegExDot.ExecutionOptions.requireMatchList executionOptions
					then show . (RegExDot.RegEx.+~ regExOpts)	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
					else show . (RegExDot.RegEx.=~ regExOpts)	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
			) &&& show . ExtendedRegExChar.MkExtendedRegExChar False {-hasNonCapturingTopLevelAlternatives-}
		 ) . testGenerator

