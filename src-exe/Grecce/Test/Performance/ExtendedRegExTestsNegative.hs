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

	* Defines 'Performance.ExtendedRegExTest.TestGenerator's, for 'Performance.ExtendedRegExTest.Test's, which should fail.

	* Implements 'Performance.ExtendedRegExTest.TestGeneratorRepository'.
-}

module Grecce.Test.Performance.ExtendedRegExTestsNegative(
-- * Types
-- ** Data-types
	TestName
) where

import qualified	Control.Arrow
import qualified	Data.Maybe
import qualified	Grecce.Test.Performance.ExtendedRegExTest		as Test.Performance.ExtendedRegExTest
import qualified	Grecce.Test.Performance.ExtendedRegExTestsPositive	as Test.Performance.ExtendedRegExTestsPositive

-- | Defines an enumeration of keys, by which to reference 'Test.Performance.ExtendedRegExTest.TestGenerator's.
data TestName	=
	INTERLEAVED_GREEDY_STAR_VS_MANDATORY
	| LINEAR_FRACTAL_NONGREEDY_STAR
	| OVERLAPPING_ALTERNATIVES
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
 Explanation:	based on 'Test.Performance.ExtendedRegExTestsPositive.INTERLEAVED_GREEDY_STAR_VS_MANDATORY', but with twice the input data following by a mismatch.
 Performance:	vastly better performance than "Text.Regex.Posix", benefiting from 'ExecutionOptions.checkExistenceOfInelasticTail'.
-}
			(
				INTERLEAVED_GREEDY_STAR_VS_MANDATORY,
				Control.Arrow.first ((++ "!") . concat . replicate 2) . Data.Maybe.fromJust (
					Test.Performance.ExtendedRegExTest.query Test.Performance.ExtendedRegExTestsPositive.INTERLEAVED_GREEDY_STAR_VS_MANDATORY
				)
			),
{-
 Explanation:	based on 'Test.Performance.ExtendedRegExTestsPositive.LINEAR_FRACTAL_NONGREEDY_STAR', but with mismatching input data, which causes the time-complexity explodes to O(n!).
 Performance:	vastly better performance than "Text.Regex.Posix", benefiting from 'ExecutionOptions.checkForUnconsumableData'.
-}
			(
				LINEAR_FRACTAL_NONGREEDY_STAR,
				Control.Arrow.first (++ "!") . Data.Maybe.fromJust (
					Test.Performance.ExtendedRegExTest.query Test.Performance.ExtendedRegExTestsPositive.LINEAR_FRACTAL_NONGREEDY_STAR
				)
			),
{-
 Explanation:	based on 'Test.Performance.ExtendedRegExTestsPositive.OVERLAPPING_ALTERNATIVES', but with mismatching input data.
 Performance:	marginally better performance than "Text.Regex.Posix", benefiting from 'ExecutionOptions.checkForUnconsumableData' & 'ExecutionOptions.permitReorderingOfAlternatives'.
-}
			(
				OVERLAPPING_ALTERNATIVES,
				Control.Arrow.first (++ "!") . Data.Maybe.fromJust (
					Test.Performance.ExtendedRegExTest.query Test.Performance.ExtendedRegExTestsPositive.OVERLAPPING_ALTERNATIVES
				)
			)
		]
	 )
