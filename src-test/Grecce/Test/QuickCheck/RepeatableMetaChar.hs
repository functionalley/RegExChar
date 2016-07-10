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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines tests based on it.
-}

module Grecce.Test.QuickCheck.RepeatableMetaChar(
-- * Constants
	results,
-- * Types
-- ** Data-types
--	RepeatableMetaChar,
-- ** Type-synonyms
--	Testable,
-- * Functions
-- ** Deconstructor
--	deconstruct
) where

import qualified	Data.Maybe
import			Grecce.Test.QuickCheck.MetaChar()
import qualified	RegExChar.MetaChar	as MetaChar
import qualified	RegExDot.Consumer
import qualified	RegExDot.ConsumptionBounds
import qualified	RegExDot.ConsumptionProfile
import qualified	RegExDot.Repeatable
import qualified	Test.QuickCheck
import qualified	ToolShed.SelfValidate
import qualified	ToolShed.Test.ReversibleIO

-- | A specialised instance, required to instantiate 'Test.QuickCheck.Arbitrary'.
newtype RepeatableMetaChar	= MkRepeatableMetaChar (RegExDot.Repeatable.Repeatable MetaChar.MetaChar)	deriving (Eq, Read, Show)

-- | Accessor.
deconstruct :: RepeatableMetaChar -> RegExDot.Repeatable.Repeatable MetaChar.MetaChar
deconstruct (MkRepeatableMetaChar repeatableMetaChar)	= repeatableMetaChar

instance ToolShed.SelfValidate.SelfValidator RepeatableMetaChar	where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstruct

instance RegExDot.Consumer.Consumer RepeatableMetaChar	where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . deconstruct
	starHeight		= RegExDot.Consumer.starHeight . deconstruct

instance Test.QuickCheck.Arbitrary RepeatableMetaChar	where
	arbitrary	= do
		metaChar	<- Test.QuickCheck.arbitrary
		fewest		<- Test.QuickCheck.elements [0 .. 9]	-- Could be more, but this is an adequate test.
		most		<- Test.QuickCheck.oneof [return Nothing, Just `fmap` Test.QuickCheck.elements [max fewest 1 .. 9]]
		isGreedy	<- Test.QuickCheck.arbitrary

		let
			repetitionBounds :: RegExDot.Repeatable.RepetitionBounds
			repetitionBounds	= (fewest, most)

		return {-to Gen-monad-} . MkRepeatableMetaChar $ RegExDot.Repeatable.MkRepeatable {
			RegExDot.Repeatable.base		= metaChar,
			RegExDot.Repeatable.repetitionBounds	= repetitionBounds,
			RegExDot.Repeatable.isGreedy		= RegExDot.Repeatable.hasPreciseBounds repetitionBounds || isGreedy	-- Only specify 'non-greedy' where space exists.
		}

type Testable	= RepeatableMetaChar -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_consumptionProfile,
	Test.QuickCheck.quickCheckResult prop_io,
	Test.QuickCheck.quickCheckResult prop_isValid,
	Test.QuickCheck.quickCheckResult prop_starHeight,
	Test.QuickCheck.quickCheckResult prop_read
 ] where
	prop_consumptionProfile, prop_io, prop_isValid, prop_starHeight :: Testable
	prop_consumptionProfile r	= Test.QuickCheck.label "prop_consumptionProfile" $ Data.Maybe.maybe True (>= minData) maybeMaxData && b == RegExDot.Consumer.getHasSpecificRequirement (RegExDot.Repeatable.base $ deconstruct r)	where
		RegExDot.ConsumptionProfile.MkConsumptionProfile {
			RegExDot.ConsumptionProfile.consumptionBounds		= (minData, maybeMaxData),
			RegExDot.ConsumptionProfile.hasSpecificRequirement	= b
		} = RegExDot.Consumer.consumptionProfile r

	prop_io (MkRepeatableMetaChar r)		= Test.QuickCheck.label "prop_io" $ ToolShed.Test.ReversibleIO.isReversible r
	prop_isValid r					= Test.QuickCheck.label "prop_isValid" $ ToolShed.SelfValidate.isValid r
	prop_starHeight (MkRepeatableMetaChar r)	= Test.QuickCheck.label "prop_starHeight" $ RegExDot.Consumer.starHeight r == if RegExDot.ConsumptionBounds.isPrecise (RegExDot.Consumer.getConsumptionBounds r) then 0 else 1

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(RepeatableMetaChar, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

