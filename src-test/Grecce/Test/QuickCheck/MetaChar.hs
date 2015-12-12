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

 [@TODO@]	Test /Perl-style shortcuts/ & /Posix Character-classes/.
-}

module Grecce.Test.QC.MetaChar(
-- * Constants
	results,
--	testCharacters,
-- * Types
-- ** Type-synonyms
--	Testable
) where

import qualified	Data.List
import qualified	RegExChar.MetaChar	as MetaChar	-- CAVEAT: beware of the similar name.
import qualified	RegExDot.BracketExpression
import qualified	RegExDot.BracketExpressionMember
import qualified	RegExDot.Meta
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO

testCharacters :: String
testCharacters	= filter (`notElem` [RegExDot.Meta.shortcutToken, RegExDot.BracketExpression.negationToken]) [' ' .. '~']	-- TODO: permit all characters.

instance Test.QuickCheck.Arbitrary MetaChar.MetaChar	where
	arbitrary	= Test.QuickCheck.oneof $ map (fmap MetaChar.MkMetaChar) [
		return {-to Gen-monad-} RegExDot.Meta.Any,
		RegExDot.Meta.Literal `fmap` Test.QuickCheck.elements testCharacters,
		(RegExDot.Meta.AnyOf . map RegExDot.BracketExpressionMember.Literal . Data.List.nub) `fmap` arbitraryCharList 1 16,	-- Test.QuickCheck.vector 16 {-Char-},
		(RegExDot.Meta.NoneOf . map RegExDot.BracketExpressionMember.Literal . Data.List.nub) `fmap` arbitraryCharList 1 4
	 ] where
		arbitraryCharList :: Int -> Int -> Test.QuickCheck.Gen String
		arbitraryCharList i j	= do
			l	<- Test.QuickCheck.arbitrary {-Int-}

			(take (min j $ i `max` l) . Data.List.nub . Data.List.filter (`elem` testCharacters)) `fmap` Test.QuickCheck.vector maxBound {-Char-}

type Testable	= MetaChar.MetaChar -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results = mapM Test.QuickCheck.quickCheckResult [prop_io]	where
	prop_io :: Testable
	prop_io	= Test.QuickCheck.label "prop_io" . ToolShed.Test.ReversibleIO.isReversible

