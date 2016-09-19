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
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines tests based on it.
-}

module Grecce.Test.QuickCheck.ExtendedRegExChar(
-- * Constants
	results,
-- * Types
-- ** Data-types
--	AlternativesChar(..),
--	PatternChar(..),
--	RepeatablePatternChar(..)
) where

import qualified	Data.Maybe
import			Grecce.Test.QuickCheck.MetaChar()
import qualified	RegExChar.ExtendedRegExChar	as ExtendedRegExChar	-- CAVEAT: beware of the similar name.
import qualified	RegExChar.MetaChar		as MetaChar
import qualified	RegExDot.Anchor
import qualified	RegExDot.BracketExpressionMember
import qualified	RegExDot.Consumer
import qualified	RegExDot.ConsumptionBounds
import qualified	RegExDot.ConsumptionProfile
import			RegExDot.DSL((-:), (?:), (+:), (<~>))
import qualified	RegExDot.Meta
import qualified	RegExDot.RegEx
import			RegExDot.RegEx((+~))
import qualified	RegExDot.RegExOpts
import qualified	RegExDot.Repeatable
import qualified	RegExDot.Result
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))
import qualified	ToolShed.SelfValidate
import qualified	ToolShed.Test.ReversibleIO

-- | A specialised instance, required to instantiate 'Test.QuickCheck.Arbitrary'.
newtype AlternativesChar	= MkAlternativesChar {
	deconstructAlternativesChar	:: RegExDot.RegEx.Alternatives Char
} deriving (Eq, Read, Show)

instance ToolShed.SelfValidate.SelfValidator AlternativesChar where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstructAlternativesChar

instance RegExDot.Consumer.Consumer AlternativesChar where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . deconstructAlternativesChar
	starHeight		= RegExDot.Consumer.starHeight . deconstructAlternativesChar

instance Test.QuickCheck.Arbitrary AlternativesChar	where
	arbitrary	= (MkAlternativesChar . RegExDot.RegEx.MkAlternatives . map ExtendedRegExChar.extendedRegEx) `fmap` (
		Test.QuickCheck.elements [1, 2]	>>= Test.QuickCheck.vector {-[ExtendedRegExChar]-}
	 ) -- TODO: permit zero alternatives.

-- | A specialised instance, required to instantiate 'Test.QuickCheck.Arbitrary'.
newtype PatternChar	= MkPatternChar {
	deconstructPatternChar	:: RegExDot.RegEx.Pattern Char
} deriving (Eq, Read, Show)

instance ToolShed.SelfValidate.SelfValidator PatternChar	where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstructPatternChar

instance RegExDot.Consumer.Consumer PatternChar	where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . deconstructPatternChar
	starHeight		= RegExDot.Consumer.starHeight . deconstructPatternChar

instance Test.QuickCheck.Arbitrary PatternChar	where
	arbitrary	= MkPatternChar `fmap` Test.QuickCheck.frequency [
		(4,	(RegExDot.RegEx.Require . MetaChar.deconstruct) `fmap` Test.QuickCheck.arbitrary {-MetaChar-}),
		(1,	(RegExDot.RegEx.CaptureGroup . deconstructAlternativesChar) `fmap` Test.QuickCheck.arbitrary {-AlternativesChar-})
	 ] -- CAVEAT: may recurse forever if 'RegExDot.RegEx.CaptureGroup / RegExDot.RegEx.Require' is too high.

-- | A specialised instance, required to instantiate 'Test.QuickCheck.Arbitrary'.
newtype RepeatablePatternChar	= MkRepeatablePatternChar {
	deconstructRepeatablePatternChar	:: RegExDot.Repeatable.Repeatable PatternChar
} deriving (Eq, Read, Show)

instance ToolShed.SelfValidate.SelfValidator RepeatablePatternChar	where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstructRepeatablePatternChar

instance RegExDot.Consumer.Consumer RepeatablePatternChar	where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . deconstructRepeatablePatternChar
	starHeight		= RegExDot.Consumer.starHeight . deconstructRepeatablePatternChar

instance Test.QuickCheck.Arbitrary RepeatablePatternChar	where
	arbitrary	= do
		patternChar	<- Test.QuickCheck.arbitrary
		fewest		<- Test.QuickCheck.elements [0 .. 9]	-- Could be more, but this is an adequate test.
		most		<- Test.QuickCheck.oneof [return {-to Gen-monad-} Nothing, Just `fmap` Test.QuickCheck.elements [max fewest 1 .. 9]]
		isGreedy	<- Test.QuickCheck.arbitrary {-Bool-}

		let
			repetitionBounds :: RegExDot.Repeatable.RepetitionBounds
			repetitionBounds	= (fewest, most)

		return {-to Gen-monad-} $ MkRepeatablePatternChar RegExDot.Repeatable.MkRepeatable {
			RegExDot.Repeatable.base		= patternChar,
			RegExDot.Repeatable.repetitionBounds	= repetitionBounds,
			RegExDot.Repeatable.isGreedy		= RegExDot.Repeatable.hasPreciseBounds repetitionBounds || isGreedy	-- Only specify non-greedy where space exists.
		}

instance Test.QuickCheck.Arbitrary ExtendedRegExChar.ExtendedRegExChar	where
	arbitrary	= do
		hasBowAnchor	<- Test.QuickCheck.arbitrary {-Bool-}
		concatenation	<- map (fmap deconstructPatternChar {-replace base-} . deconstructRepeatablePatternChar {-RegExDot.Repeatable.Repeatable PatternChar-}) `fmap` Test.QuickCheck.vector 2 {-[RepeatablePatternChar]-}
		hasSternAnchor	<- Test.QuickCheck.arbitrary {-Bool-}

		return {-to Gen-monad-} $ ExtendedRegExChar.MkExtendedRegExChar False RegExDot.RegEx.MkExtendedRegEx {
			RegExDot.RegEx.bowAnchor	= if hasBowAnchor then Just RegExDot.Anchor.Bow else Nothing,
			RegExDot.RegEx.concatenation	= concatenation,
			RegExDot.RegEx.sternAnchor	= if hasSternAnchor then Just RegExDot.Anchor.Stern else Nothing
		}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_consumptionProfile,
	Test.QuickCheck.quickCheckResult prop_consumptionProfile2,
	Test.QuickCheck.quickCheckResult prop_io,
	Test.QuickCheck.quickCheckResult prop_io',
	Test.QuickCheck.quickCheckResult prop_isValid,
	Test.QuickCheck.quickCheckResult prop_starHeight,
	Test.QuickCheck.quickCheckResult prop_double,
	Test.QuickCheck.quickCheckResult prop_read
 ] where
	prop_consumptionProfile, prop_consumptionProfile2, prop_io, prop_io', prop_isValid, prop_starHeight :: ExtendedRegExChar.ExtendedRegExChar -> Test.QuickCheck.Property
	prop_consumptionProfile r	= Test.QuickCheck.label "prop_consumptionProfile" $ Data.Maybe.maybe True (>= minData) maybeMaxData && (
		not b == all (not . RegExDot.Consumer.getHasSpecificRequirement) (RegExDot.RegEx.concatenation $ ExtendedRegExChar.extendedRegEx r)
	 ) where
		RegExDot.ConsumptionProfile.MkConsumptionProfile {
			RegExDot.ConsumptionProfile.consumptionBounds		= (minData, maybeMaxData),
			RegExDot.ConsumptionProfile.hasSpecificRequirement	= b
		} = RegExDot.Consumer.consumptionProfile r

	prop_consumptionProfile2 r	= RegExDot.RegEx.isDefined (ExtendedRegExChar.extendedRegEx r)	==> Test.QuickCheck.label "prop_consumptionProfile2" $ hasSpecificRequirement || canConsumeAnything	where	-- There's either; a requirement for at least one specific, or we can consume at least one arbitrary; input datum.
		RegExDot.ConsumptionProfile.MkConsumptionProfile {
			RegExDot.ConsumptionProfile.hasSpecificRequirement	= hasSpecificRequirement,
			RegExDot.ConsumptionProfile.canConsumeAnything		= canConsumeAnything
		} = RegExDot.Consumer.consumptionProfile r

	prop_io			= Test.QuickCheck.label "prop_io" . ToolShed.Test.ReversibleIO.isReversible
	prop_io' r		= Test.QuickCheck.label "prop_io'" $ ToolShed.Test.ReversibleIO.isReversible r'	where r' = ExtendedRegExChar.extendedRegEx r	-- Check "RegExDot.RegEx.ExtendedRegEx Char" too.
	prop_isValid r		= Test.QuickCheck.label "prop_isValid" $ ToolShed.SelfValidate.isValid r
	prop_starHeight r	= Test.QuickCheck.label "prop_starHeight" $ (RegExDot.Consumer.starHeight r == 0) == RegExDot.ConsumptionBounds.isPrecise (RegExDot.Consumer.getConsumptionBounds r)

	prop_double :: Double -> Test.QuickCheck.Property
	prop_double d	= Test.QuickCheck.label "prop_double" $ RegExDot.RegEx.extractDataFromMatchList `fmap` RegExDot.Result.getMatchList result == Just s	where
		s :: String
		s	= show $ d * 1e6	-- Implementation of show, uses exponential notation, when the number is large.

		result :: RegExDot.RegEx.Result Char
		result = s +~ RegExDot.RegExOpts.mkRegEx (
			(Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~> sign ?: digits +: map (
				RegExDot.Repeatable.zeroOrOne . RegExDot.RegEx.captureGroup . map (RegExDot.Anchor.unanchored <~>) . return {-to List-monad-}
			) [
				RegExDot.RegEx.Require (RegExDot.Meta.Literal '.') -: digits +: [],
				RegExDot.RegEx.Require (RegExDot.Meta.AnyOf $ map RegExDot.BracketExpressionMember.Literal "eE") -: sign ?: digits +: []
			]
		 ) where
			sign, digits :: RegExDot.RegEx.Pattern Char
			sign	= RegExDot.RegEx.Require . RegExDot.Meta.AnyOf $ map RegExDot.BracketExpressionMember.Literal "+-"
			digits	= RegExDot.RegEx.Require . RegExDot.Meta.AnyOf $ map RegExDot.BracketExpressionMember.Literal ['0' .. '9']

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(ExtendedRegExChar.ExtendedRegExChar, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

