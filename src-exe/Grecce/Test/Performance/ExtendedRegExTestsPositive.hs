{-
	Copyright (C) 2010 Dr. Alistair Ward

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

	* Defines 'Performance.ExtendedRegExTest.TestGenerator's, for 'Performance.ExtendedRegExTest.Test's, which should pass.

	* Implements 'Performance.ExtendedRegExTest.TestGeneratorRepository'.
-}

module Grecce.Test.Performance.ExtendedRegExTestsPositive(
-- * Types
-- ** Data-types
	TestName(..)
-- * Functions
--	triangularSeries
) where

import			Control.Arrow((&&&))
import qualified	Data.Char
import qualified	Grecce.Test.Performance.ExtendedRegExTest	as Test.Performance.ExtendedRegExTest
import qualified	RegExDot.Anchor
import qualified	RegExDot.BracketExpressionMember
import			RegExDot.DSL((-:), (?:), (*:), (<~>))
import qualified	RegExDot.Meta
import qualified	RegExDot.RegEx
import			RegExDot.RegEx((.*), (.*?))
import qualified	RegExDot.Repeatable
import			RegExDot.Repeatable((^#->#), (^#->), (^#))

-- | Defines an enumeration of keys, by which to reference 'Test.Performance.ExtendedRegExTest.TestGenerator's.
data TestName	=
	BIPOLAR_GREEDY_OPTIONAL_VS_MANDATORY
	| INTERLEAVED_GREEDY_STAR_VS_MANDATORY
	| TRIANGULAR_CONSUMPTION
	| BINARY_TREE
	| REPEATABLE_ALTERNATIVES
	| REPEATED_ALTERNATIVES
	| LINEAR_FRACTAL_RANGES
	| LINEAR_FRACTAL_NONGREEDY_STAR
	| BINARY_TREE_FRACTAL
	| FUTILE_REPEATABLE_ALTERNATIVES
	| OVERLAPPING_ALTERNATIVES
	| UNBRIDLED_GREED
	| MULTIPLE_CHOICE
	deriving (
		Bounded,
		Enum,
		Eq,
		Read,
		Show
	)

instance Test.Performance.ExtendedRegExTest.TestGeneratorRepository TestName	where
	query	= (
		`lookup` [
{-
 "^(<testChar>?){<complexity>}(<testChar>){<complexity>}$".
 Explanation:	each of the optional meta-data in the first half of the expression can capture data, but in doing so starves a mandatory meta-datum in the second half of the expression,
		presenting the opportunity for O(2^n) time-complexity backtracking.
 Performance:	vastly better Boolean-result performance than "Text.Regex.Posix". Looks like O(n^2) time-complexity.
-}
			(
				BIPOLAR_GREEDY_OPTIONAL_VS_MANDATORY,
				(`replicate` testChar) &&& let
					exactly :: Test.Performance.ExtendedRegExTest.TestComplexity -> (RegExDot.RegEx.Pattern Char -> RegExDot.RegEx.RepeatablePattern Char) -> RegExDot.RegEx.RepeatablePattern Char
					exactly n f	= (RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} . return {-to List-monad-} $ f testRequirement) ^# n
				in ((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . (`map` [RegExDot.Repeatable.zeroOrOne, RegExDot.Repeatable.one]) . exactly
			),
{-
 "^(.*z){<complexity>}$".
 Explanation:	each '.*' can consume all the remaining data, starving the interspersed mandatory meta data, presenting the opportunity for O(2^n) time-complexity backtracking.
 Performance:	marginally better Boolean-result & data-capture performance than "Text.Regex.Posix" with 'ExecutionOptions.moderateGreed'. Empirically O(n^3) time-complexity.
-}
			(
				INTERLEAVED_GREEDY_STAR_VS_MANDATORY,
				concat . (`replicate` (testChar : "z")) &&& RegExDot.RegEx.dock . (
					`mkExtendedRegExFromRepeatableSingletonAlternative` (RegExDot.Anchor.unanchored <~>) [
						(.*),
						RegExDot.RegEx.simply $ RegExDot.Meta.Literal 'z'
					]
				) . flip (^#)
			),
{-
 "^<testChar>*<testChar>+<testChar>{2,}<testChar>{3,} ... a{<complexity>,}$".
 Explanation:	each term greedily consumes all the data, starving all the remaining terms, until after much back-tracking, it consumes the just the minimum permissible.
		The correct consumption-pattern is triangular, starting at the apex & spreading into a delta.
 Performance:	vastly better Boolean-result & data-capture performance than "Text.Regex.Posix".
-}

			(
				TRIANGULAR_CONSUMPTION,
				(`replicate` testChar) . triangularSeries &&& ((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . (`take` map (testRequirement ^#->) [0 ..]) . succ
			),
{-
 "(<testChar>|<testChar>{2})*c".
 Explanation:	from <http://www.haskell.org/pipermail/libraries/2009-March/011379.html>
 Performance:	disastrous performance.
-}
			(
				BINARY_TREE,
				(++ "bc") . (`replicate` testChar) . (2 ^) &&& const (
					RegExDot.Anchor.unanchored <~> RegExDot.RegEx.captureGroup (
						map (
							(RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} . (testRequirement ^#) . (2 ^)
						) [0 :: Int .. 1]
					) *: RegExDot.RegEx.Require (RegExDot.Meta.Literal 'c') -: []
				)
			),
{-
 "^(<testChar>?|<testChar>{2}|<testChar>{4}|<testChar>{8}| ... |<testChar>{2^<complexity>})*$".
 Explanation:	all but one of the 'Alternatives' must be used, one on each successive repetition, in order to consume all the 'InputData'.
		A correct implementation will match against the longest possible Alternative on each successive repetition, just as if the repetition were unrolled; see 'REPEATED_ALTERNATIVES'.
 Performance:	vastly worse Boolean-result performance (even with 'ExecutionOptions.permitReorderingOfAlternatives'),
		& worse data-capture performance (even with 'ExecutionOptions.useFirstMatchAmongAlternatives'), than "Text.Regex.Posix".
-}
			(
				REPEATABLE_ALTERNATIVES,
				(`replicate` testChar) . pred {-requires several different Alternatives-} . (2 ^) &&& (
					\i -> ((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . return {-to List-monad-} . RegExDot.Repeatable.zeroOrMore . RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) $ [RegExDot.Repeatable.zeroOrOne testRequirement] : map (
						return {-to List-monad-} . (testRequirement ^#) . (2 ^)
					) [1 .. i]
				)
			),
{-
 "^(<testChar>?|<testChar>{2}|<testChar>{4}| ... |<testChar>{2^<complexity>})(<testChar>?|<testChar>{2}|<testChar>{4}| ... |<testChar>{2^<complexity>})(<testChar>?|<testChar>{2}|<testChar>{4}| ... |<testChar>{2^<complexity>})$".
 Explanation:	'REPEATABLE_ALTERNATIVES' with the repetition unrolled, to permit inspection of individual capture-groups,
		revealing the use by "Text.Regex.Posix", of Perl-style "1st-past-the-post" selection between Alternatives.
 Performance:	much faster than 'REPEATABLE_ALTERNATIVES'. Vastly better Boolean-result & data-capture performance than both 'REPEATABLE_ALTERNATIVES' & "Text.Regex.Posix".
-}
			(
				REPEATED_ALTERNATIVES,
				(`replicate` testChar) . pred {-requires several different Alternatives-} . (2 ^) &&& (
					\i -> ((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . replicate i . RegExDot.Repeatable.one . RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) $ [RegExDot.Repeatable.zeroOrOne testRequirement] : map (
						return {-to List-monad-} . (testRequirement ^#) . (2 ^)
					) [1 .. i]
				)
			),
{-
 "^((((<testChar>{0,2}){0,2}){0,2}){0,2}){0,2} ... $".
 Explanation:
 Performance:	vastly better Boolean-result performance than "Text.Regex.Posix", which exhausts memory at n==10. Better data-capture performance too.
		Stack-overflow at 'Test.Performance.ExtendedRegExTest.TestComplexity'=14.
-}
			(
				LINEAR_FRACTAL_RANGES,
				let
					base :: RegExDot.Repeatable.Repetitions
					base	= 2	-- Arbitrarily.

					bounds :: RegExDot.Repeatable.RepetitionBounds
					bounds	= (0, Just base)
				in (`replicate` testChar) . (base ^) &&& RegExDot.RegEx.dock . (
					(
						mkExtendedRegExFromRepeatableSingletonAlternative (^#-># bounds) `iterate` (RegExDot.Anchor.unanchored <~>) [testRequirement ^#-># bounds] {-initial value-}
					) !!
				) . pred
			),
{-
 "^((((((((((((((<testChar>*?)*?)*?)*?)*?)*?)*?)*?)*?)*?)*?)*?)*?)*?)*? ... $".
 Explanation:	interesting because all 'RegExDot.RegEx.Alternatives' are expanded before the first comparison against input data, & the number possible repetitions of each expansion is very vague.
 Performance:	vastly better Boolean-result performance than "Text.Regex.Posix", but inferior data-capture performance.
-}
			(
				LINEAR_FRACTAL_NONGREEDY_STAR,
				const (replicate 8 {-arbitrary-} testChar) &&& RegExDot.RegEx.dock . (
					(
						mkExtendedRegExFromRepeatableSingletonAlternative RegExDot.Repeatable.zeroOrMore' `iterate` (RegExDot.Anchor.unanchored <~>) [RegExDot.Repeatable.zeroOrMore' testRequirement] {-initial value-}
					) !!
				)
			),
{-
 "^((([^<testChar>]|<testChar>)|(<testChar>|[^<testChar>]))|((<testChar>|[^<testChar>])|([^<testChar>]|<testChar>)))$".
 Explanation:	exhibits symmetry in the left & right branches of the binary tree of RegExDot.RegEx.Alternatives, & so the exponentially expanding set of leaves are equivalent & all match the data.
 Performance:	vastly better Boolean-result & comparable data.capture performance to "Text.Regex.Posix", which exhausts memory at complexity 5 to 6, & to egrep which lasts until complexity==6.
-}
			(
				BINARY_TREE_FRACTAL,
				let
					base :: RegExDot.Repeatable.Repetitions
					base	= 2	-- Arbitrarily.
				in (`replicate` testChar) . (base ^) &&& RegExDot.RegEx.dock . head . (
					(
						iterate (
							\extendedRegExList -> (
								(RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} . (^# base) . RegExDot.RegEx.captureGroup
							) `map` [extendedRegExList, reverse extendedRegExList]
						) $ map (
							(RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} . ($ RegExDot.RegEx.Require $ RegExDot.Meta.Literal testChar)
						) [RegExDot.Repeatable.one, RegExDot.Repeatable.zeroOrOne]
					) !!
				)
			),
{-
 "^(<testChar>1|<testChar>{2}2|<testChar>{3}3| ... |<testChar>{<complexity>}<complexity>)*<testChar>*$".
 Explanation:	none of the Alternatives can match, & consequently the group must match zero times, presenting the opportunity for many wasted trial repetitions.
 Performance:	comparable Boolean-result performance to "Text.Regex.Posix", & marginally superior data-capture performance.
-}
			(
				FUTILE_REPEATABLE_ALTERNATIVES,
				\alternatives	-> (
					replicate 1024 {-arbitrarily-} testChar,
					((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) $ map RegExDot.Repeatable.zeroOrMore [
						RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) $ (
							\i -> ($ i) `map` [
								(testRequirement ^#),
								RegExDot.RegEx.simply . RegExDot.Meta.Literal . Data.Char.chr . (Data.Char.ord '0' +) . (`mod` 10)
							]
						) `map` [1 .. alternatives],
						testRequirement
					]
				)
			),
{-
 "^a?((ab?)*|(bc?)*|(cd?)*| ...)*$".
 Explanation:	the zeroeth term matches greedily, consuming the datum mandated by the 1st Alternative, so the 2nd Alternative greedily consumes two data, including the datum mandated by the 3rd, ...
		If there're an even number of Alternatives, then n/2 repetitions (one for each), will consume all the input data, but otherwise the final input datum isn't consumed.
		The consumption back-tracks rapidly until the zeroeth term yields-up the initial datum, after which the odd Alternatives match consuming all the input data, in (n/2 + 1) repetitions.
		This suggests a very tractable O(n) time-complexity, but Posix requires that we select the combination of Alternatives which consume most data,
		& since there are two terms in the regex that can consume each input datum, it looks like there should be 2^n candidate solutions; in fact there's just one.
		So, all O(Alternatives ^ repetitions) permutations may be searched in vain for a superior solution.
 Performance:	vastly worse Boolean-result performance than "Text.Regex.Posix", but the latter then stalls indefinitely (CPU-bound) at complexity=[13 (not verbose), 4 (verbose)].
-}
			(
				OVERLAPPING_ALTERNATIVES,
				let
					bracket :: a -> [a] -> [a]
					bracket terminal	= (terminal :) . (++ [terminal])

					zeroOrMoreAlternatives :: [RegExDot.RegEx.Concatenation Char] -> RegExDot.RegEx.Concatenation Char
					zeroOrMoreAlternatives	= return {-to List-monad-} . RegExDot.Repeatable.zeroOrMore . RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>)
				in (
					id &&& (
						((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . (RegExDot.RegEx.Require (RegExDot.Meta.Literal testChar) ?:) . zeroOrMoreAlternatives . map (
							\(a, b)	-> zeroOrMoreAlternatives $ return {-to List-monad-} [a, b]
						) . init . tail . uncurry zip . (
							map RegExDot.Repeatable.one . init &&& map RegExDot.Repeatable.zeroOrOne . tail
						) . bracket undefined . map (
							RegExDot.RegEx.Require . RegExDot.Meta.Literal
						)
					)
				) . (`take` (filter Data.Char.isAlphaNum $ filter Data.Char.isAscii [minBound .. maxBound])) . succ
			),
{-
 "^(a|b|c)*(a.*?c){3}$".
 Explanation:	zero repetitions of the set of Alternatives on the LHS is the only solution, but because the greedy star, within which they're grouped, permits many more,
		& because the RHS's requirement for all the input-data, isn't explicit, there's a temptation to over-consume.
 Performance:	empirically O(2^n) time-complexity, but linearised by 'ExecutionOptions.moderateGreed'.
		Better Boolean-result & data-capture performance than "Text.Regex.Posix".
-}
			(
				UNBRIDLED_GREED,
				\complexity	-> let
					complexity' :: Test.Performance.ExtendedRegExTest.TestComplexity
					complexity'	= succ complexity

					testSet :: String
					testSet	= take complexity' . filter Data.Char.isAlphaNum $ filter Data.Char.isAscii [minBound .. maxBound]
				in (
					concat $ replicate complexity' testSet,
					((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) $ (
						RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) $ map (
							return {-to List-monad-} . RegExDot.RegEx.simply . RegExDot.Meta.Literal
						) testSet
					) *: [
						(
							RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} $ RegExDot.RegEx.Require (RegExDot.Meta.Literal $ head testSet) -: (.*?) : RegExDot.RegEx.Require (RegExDot.Meta.Literal $ last testSet) -: []
						) ^# complexity'
					]
				)
			),
{-
 "^(a*[^a]|[^b]|[^c]| ... |[^z])*$"
 Explanation:	The first alternative greedily consumes all the input data until finding a mismatch on the last, whilst all the remaining alternatives successfully consume a single input datum.
		The same occurs on each successive repetition of the alternatives, of which there are as many as input data,
		leading to a lot of wasted time with O(<alternatives> ^ <input data>) time-complexity.
 Performance:	poor Boolean-result & worse data-capture performance (benefiting from 'ExecutionOptions.useFirstMatchAmongAlternatives'), compared to "Text.Regex.Posix".
-}
			(
				MULTIPLE_CHOICE,
				const (replicate 8 {-arbitrary-} testChar) &&& (
					((Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~>) . return {-to List-monad-} . RegExDot.Repeatable.zeroOrMore . RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) . (
						[
							RegExDot.Repeatable.zeroOrMore testRequirement,
							RegExDot.RegEx.simply . RegExDot.Meta.NoneOf $ map RegExDot.BracketExpressionMember.Literal [testChar]
						] :
					) . map (
						return {-to List-monad-} . RegExDot.RegEx.simply . RegExDot.Meta.NoneOf . return {-to List-monad-} . RegExDot.BracketExpressionMember.Literal
					) . (
						`take` tail [testChar ..]
					)
				)
			)
		]
	 ) where
		testChar :: Char
		testChar	= 'a'

		testRequirement :: RegExDot.RegEx.Pattern Char
		testRequirement	= RegExDot.RegEx.Require $ RegExDot.Meta.Literal testChar

		mkExtendedRegExFromRepeatableSingletonAlternative :: (RegExDot.RegEx.Pattern Char -> RegExDot.RegEx.RepeatablePattern Char) -> RegExDot.RegEx.ExtendedRegEx Char -> RegExDot.RegEx.ExtendedRegEx Char
		mkExtendedRegExFromRepeatableSingletonAlternative makeRepetitions	= (RegExDot.Anchor.unanchored <~>) . return {-to List-monad-} . makeRepetitions . RegExDot.RegEx.captureGroup . return {-to List-monad-}

{- |
	* The sum of the sequence descending from the specified integer; @n + (n - 1) + (n - 2) + ... + 2 + 1 + 0@.

	* <http://en.wikipedia.org/wiki/Triangular_number>.

	* <http://mathworld.wolfram.com/TriangularNumber.html>.
-}
triangularSeries :: Integral i => i -> i
triangularSeries i	= i * succ i `div` 2
