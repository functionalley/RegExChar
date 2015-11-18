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

	* Defines the types used for performance-testing.

	* Defines an interface to permit queries on repositories of 'TestGenerator's.
-}

module Grecce.Test.Performance.ExtendedRegExTest(
-- * Type-classes
	TestGeneratorRepository(..),
-- * Types
-- ** Type-synonyms
	TestComplexity,
	Test,
	TestGenerator
) where

import qualified	RegExDot.RegEx

-- | Used to specify the level of complexity of /regex/ to generate.
type TestComplexity	= Int

-- | A pair defining the data & the /regex/ to which it should be fed.
type Test a	= (RegExDot.RegEx.InputData a, RegExDot.RegEx.ExtendedRegEx a)

-- | The type of a function, which accepts a 'TestComplexity', & returns a 'Test'.
type TestGenerator a	= TestComplexity -> Test a

-- | Implementer contracts to generate an appropriate 'Test'.
class TestGeneratorRepository a	where
	query	:: a -> Maybe (TestGenerator Char)	-- CAVEAT: polymorphism reduced, to avoid requirement for non-portable Haskell-extensions.

