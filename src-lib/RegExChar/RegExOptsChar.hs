{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

 [@DESCRIPTION@]	Creates a /back-end/ implementation, conforming to <https://hackage.haskell.org/packages/archive/regex-base/latest/doc/html/Text-Regex-Base-RegexLike.html>.

 [@CAVEATS@]

	* The underlying polymorphic (in terms of the base-type of the list of input data) 'RegExDot.RegEx.ExtendedRegEx'-engine is never going to be a drop-in replacement for other /regex/-engines,
	so this standard interface has only been implemented for traditional input data-type ['Char'].

	* The standard interface requires many Haskell-extensions, but since this implementation is just a thin layer over the underlying portable polymorphic 'RegExDot.RegEx.ExtendedRegEx'-engine,
	the latter can still be used directly, where any of these extensions are unavailable.
-}

module RegExChar.RegExOptsChar(
-- * Types
-- ** Type-synonyms
	MatchSpan,
--	MatchDataSpan,
	RegExOptsChar,
-- * Functions
--	exciseNonCapturingTopLevelAlternatives,
--	matchUntilFailure,
--	toMatchDataSpanList,
	toZeroIndexedArray,
-- ** Operators
	(=~)
-- ** Predicates
--	complyStrictlyWithPosix,
--	hasNonCapturingTopLevelAlternatives
) where

import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	RegExChar.ExtendedRegExChar	as ExtendedRegExChar
import qualified	RegExDot.CompilationOptions
import qualified	RegExDot.ConsumptionBounds
import qualified	RegExDot.DataSpan
import qualified	RegExDot.DataSpanTree
import qualified	RegExDot.ExecutionOptions
import qualified	RegExDot.RegEx
import qualified	RegExDot.RegExOpts
import qualified	RegExDot.Result
import qualified	RegExDot.Tree
import qualified	Text.Regex.Base.RegexLike	as RegexLike
import			Text.Regex.Base.Context()	-- Instance-declarations.
import qualified	ToolShed.Data.List
import qualified	ToolShed.Options

infix 4 =~	-- Same as (==).

-- | Defines a specific instance of the polymorphic base-type.
type RegExOptsChar	= RegExDot.RegExOpts.RegExOpts ExtendedRegExChar.ExtendedRegExChar

-- | Convenience accessor-function.
hasNonCapturingTopLevelAlternatives :: RegExOptsChar -> Bool
hasNonCapturingTopLevelAlternatives	= ExtendedRegExChar.hasNonCapturingTopLevelAlternatives . RegExDot.RegExOpts.regEx

-- | Convenience accessor-function.
complyStrictlyWithPosix :: RegExOptsChar -> Bool
complyStrictlyWithPosix	= RegExDot.CompilationOptions.complyStrictlyWithPosix . RegExDot.RegExOpts.compilationOptions

instance RegexLike.RegexOptions RegExOptsChar RegExDot.CompilationOptions.CompilationOptions RegExDot.ExecutionOptions.ExecutionOptions	where
	blankCompOpt	= ToolShed.Options.blankValue
	blankExecOpt	= ToolShed.Options.blankValue
	defaultCompOpt	= Data.Default.def
	defaultExecOpt	= Data.Default.def
	setExecOpts e r	= r { RegExDot.RegExOpts.executionOptions = e }
	getExecOpts	= RegExDot.RegExOpts.executionOptions

-- Newer versions of this 'Text.Regex.Base.RegexLike.RegexMaker' have additional monadic methods, which can fail on parse-errors.
instance RegexLike.RegexMaker RegExOptsChar RegExDot.CompilationOptions.CompilationOptions RegExDot.ExecutionOptions.ExecutionOptions String	where
	makeRegexOpts c e source = RegExDot.RegExOpts.MkRegExOpts {
		RegExDot.RegExOpts.compilationOptions	= c,
		RegExDot.RegExOpts.executionOptions	= e,
		RegExDot.RegExOpts.regEx		= read source
	}

-- | The offset and length of the 'RegExDot.RegEx.InputData' consumed in one 'RegExDot.RegEx.Match'.
type MatchSpan		= (RegexLike.MatchOffset, RegexLike.MatchLength)

{- |
	* The 'MatchSpan' augmented by the consumed 'ExtendedRegExChar.InputData' to which it refers.

	* Similar to 'RegExDot.DataSpan.DataSpan'.
-}
type MatchDataSpan	= (ExtendedRegExChar.InputData, MatchSpan)

{- |
	* Convert the 'RegExDot.RegEx.MatchList', into the 'MatchDataSpan's required by "Text.Regex.Base.RegexLike".

	* Treat the whole 'ExtendedRegExChar.ExtendedRegExChar' as an additional implicit capture-group.

	* CAVEAT: @RegExDot.DataSpanTree.extractCaptureGroups True@ may return some 'RegExDot.DataSpan.DataSpan's containing the artificial offset @-1@ specified by Posix => don't use this as the basis for any arithmetic.
-}
toMatchDataSpanList
	:: Bool
	-> RegExDot.ConsumptionBounds.DataLength	-- ^ The offset to use for zero-length match.
	-> RegExDot.RegEx.MatchList Char		-- ^ The list of matches.
	-> [MatchDataSpan]
toMatchDataSpanList _ offset []				= [RegExDot.DataSpan.empty offset]	-- The whole regex matched, but consumed nothing.
toMatchDataSpanList strictPosixCompliance _ matchList	= RegExDot.DataSpanTree.extractCaptureGroups strictPosixCompliance . return {-to List-monad-} . RegExDot.Tree.Node . return {-to List-monad-} $ RegExDot.DataSpanTree.toTreeList matchList

-- | Optionally remove the 2nd element from the list, where it represents the data captured by a groups of top-level 'Alternatives', which weren't explicitly delimited & therefore are non-capturing.
exciseNonCapturingTopLevelAlternatives :: RegExOptsChar -> [MatchDataSpan] -> [MatchDataSpan]
exciseNonCapturingTopLevelAlternatives regExOptsChar
	| hasNonCapturingTopLevelAlternatives regExOptsChar	= ToolShed.Data.List.excise 1	-- The zeroeth represents the whole regex, the 1st represents the top-level non-capturing group of Alternatives, & the remainder represent explicitly delimited groups.
	| otherwise						= id

-- | Convert the specified list, into a zero-indexed array.
toZeroIndexedArray :: [e] -> Data.Array.IArray.Array Int e
toZeroIndexedArray l	= Data.Array.IArray.listArray (0, pred $ length l) l

{- |
	* Repeatedly apply the 'RegExOptsChar' to the 'ExtendedRegExChar.InputData', forwarding unmatched input data to the next match-attempt, until it's all been consumed.

	* The offsets, from the start of the input data, of all matches after the first, are shifted to account for input data already consumed by previous matches.
-}
matchUntilFailure
	:: RegExOptsChar		-- ^ The match-criteria.
	-> ExtendedRegExChar.InputData	-- ^ The input-data, to be consumed by repeated matching.
	-> [RegExDot.RegEx.MatchList Char]
matchUntilFailure regExOptsChar inputData	= shiftOffsets 0 $ matchUntilFailure' inputData	where
	matchUntilFailure' :: ExtendedRegExChar.InputData -> [RegExDot.RegEx.MatchList Char]
	matchUntilFailure' unmatchedInputData	= case unmatchedInputData ExtendedRegExChar.+~ regExOptsChar of	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
		(_, Just matchList, maybeSternAnchorResult)	-> matchList : if null (RegExDot.RegEx.extractDataFromMatchList matchList) then [] else matchUntilFailure' (RegExDot.RegEx.extractDataFromMatch' maybeSternAnchorResult)
		_						-> []

	shiftOffsets :: RegExDot.ConsumptionBounds.DataLength -> [RegExDot.RegEx.MatchList Char] -> [RegExDot.RegEx.MatchList Char]
	shiftOffsets offset (matchList : matchLists)	= let
		shiftedMatchList :: RegExDot.RegEx.MatchList Char
		shiftedMatchList	= RegExDot.RegEx.shiftMatchList offset matchList
	 in shiftedMatchList : shiftOffsets (RegExDot.DataSpan.after . last . RegExDot.DataSpanTree.flattenTreeList 0 $ RegExDot.DataSpanTree.toTreeList shiftedMatchList) matchLists {-recurse-}
	shiftOffsets _ _				= []

instance RegexLike.RegexLike RegExOptsChar ExtendedRegExChar.InputData	where
{- E.g.:
	("xabcdxabdxxadx" RegExOptsChar.=~ "a(b(c?))*d") :: [Text.Regex.Base.RegexLike.MatchArray]
	[array (0,2) [(0,(1,4)),(1,(2,2)),(2,(3,1))],array (0,2) [(0,(6,3)),(1,(7,1)),(2,(8,0))],array (0,1) [(0,(11,2)),(1,(-1,0))]]
-}
--	matchAll :: RegExOptsChar -> ExtendedRegExChar.InputData -> [RegexLike.MatchArray]
	matchAll regExOptsChar	= map (toZeroIndexedArray . map snd {-span-} . exciseNonCapturingTopLevelAlternatives regExOptsChar . toMatchDataSpanList (complyStrictlyWithPosix regExOptsChar) 0) . matchUntilFailure regExOptsChar

{- E.g.:
	("xabcdxabdxxadx" RegExOptsChar.=~ "a(b(c?))*d") :: [Text.Regex.Base.RegexLike.MatchText String]
	[array (0,2) [(0,("abcd",(1,4))),(1,("bc",(2,2))),(2,("c",(3,1)))],array (0,2) [(0,("abd",(6,3))),(1,("b",(7,1))),(2,("",(8,0)))],array (0,1) [(0,("ad",(11,2))),(1,("",(-1,0)))]]
-}
--	matchAllText :: RegExOptsChar -> ExtendedRegExChar.InputData -> [RegexLike.MatchText ExtendedRegExChar.InputData]
	matchAllText regExOptsChar	= map (toZeroIndexedArray . exciseNonCapturingTopLevelAlternatives regExOptsChar . toMatchDataSpanList (complyStrictlyWithPosix regExOptsChar) 0) . matchUntilFailure regExOptsChar

{- E.g.:
	("xabcdx" RegExOptsChar.=~ "a(b(c?))*d") :: Text.Regex.Base.RegexLike.MatchArray
	array (0,2) [(0,(1,4)),(1,(2,2)),(2,(3,1))]
-}
--	matchOnce :: RegExOptsChar -> ExtendedRegExChar.InputData -> Maybe RegexLike.MatchArray
	matchOnce regExOptsChar inputData	= (
		toZeroIndexedArray . map snd {-span-} . exciseNonCapturingTopLevelAlternatives regExOptsChar . toMatchDataSpanList (
			complyStrictlyWithPosix regExOptsChar
		) (
			RegExDot.RegEx.externalMatchLength $ RegExDot.Result.getPreMatch extendedRegExResult
		)
	 ) `fmap` RegExDot.Result.getMatchList extendedRegExResult	where
		extendedRegExResult :: RegExDot.RegEx.Result Char
		extendedRegExResult	= inputData ExtendedRegExChar.+~ regExOptsChar	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.

{- E.g.:
	("xabcdx" RegExOptsChar.=~ "a(b(c?))*d") :: (String, Text.Regex.Base.RegexLike.MatchText String, String)
	("x",array (0,2) [(0,("abcd",(1,4))),(1,("bc",(2,2))),(2,("c",(3,1)))],"x")
-}
--	matchOnceText :: RegExOptsChar -> ExtendedRegExChar.InputData -> Maybe (ExtendedRegExChar.InputData, MatchText ExtendedRegExChar.InputData, ExtendedRegExChar.InputData)
	matchOnceText regExOptsChar inputData	= case inputData ExtendedRegExChar.+~ regExOptsChar of	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.
		(maybeBowAnchorResult, Just matchList, maybeSternAnchorResult)	-> Just (
			RegExDot.RegEx.extractDataFromMatch' maybeBowAnchorResult,
			toZeroIndexedArray . exciseNonCapturingTopLevelAlternatives regExOptsChar $ toMatchDataSpanList (
				complyStrictlyWithPosix regExOptsChar
			) (
				RegExDot.RegEx.externalMatchLength maybeBowAnchorResult
			) matchList,
			RegExDot.RegEx.extractDataFromMatch' maybeSternAnchorResult
		 )
		_								-> Nothing

{- E.g.:
	("xabcdx" RegExOptsChar.=~ "a(b(c?))*d") :: Bool
	True
-}
--	matchTest :: RegExOptsChar -> ExtendedRegExChar.InputData -> Bool
	matchTest	= flip (ExtendedRegExChar.=~)	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.

{- |
	* Match-operator.

	* The polymorphic return-type is resolved by the caller's "RegexLike.RegexContext".
-}
(=~) :: RegexLike.RegexContext RegExOptsChar ExtendedRegExChar.InputData target
	=> ExtendedRegExChar.InputData	-- ^ The input data.
	-> String			-- ^ The string from which to read the regex-specification.
	-> target			-- ^ The polymorphic return-type.
inputData =~ s	= (RegexLike.makeRegex s :: RegExOptsChar) `RegexLike.match` inputData

