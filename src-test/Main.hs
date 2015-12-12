{-
	Copyright (C) 2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Calls the /quickChecks/-functions for modules supporting this feature.
-}

module Main(main) where

import qualified	Control.Monad
import qualified	Grecce.Test.QuickCheck.ExtendedRegExChar	as Test.QuickCheck.ExtendedRegExChar
import qualified	Grecce.Test.QuickCheck.MetaChar			as Test.QuickCheck.MetaChar
import qualified	Grecce.Test.QuickCheck.RepeatableMetaChar	as Test.QuickCheck.RepeatableMetaChar
import qualified	System.Exit
import qualified	ToolShed.Test.QuickCheck.Result

-- | Entry-point.
main :: IO ()
main	= mapM_ (
	(`Control.Monad.unless` System.Exit.exitFailure) . all ToolShed.Test.QuickCheck.Result.isSuccessful =<<
 ) [
	Test.QuickCheck.MetaChar.results,
	Test.QuickCheck.RepeatableMetaChar.results,
	Test.QuickCheck.ExtendedRegExChar.results
 ]

