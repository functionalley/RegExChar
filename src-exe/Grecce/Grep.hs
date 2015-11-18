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

 [@DESCRIPTION@]	Performs the match-operation on the specified file-list, using the user's command-line options.
-}

module Grecce.Grep(
-- * Types
--	LineNumber,
--	Matches,
-- ** Type-synonyms
-- * Functions
	grep
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.Monad
import qualified	Control.Parallel.Strategies
import qualified	Data.List
import qualified	Grecce.CommandOptions		as CommandOptions
import qualified	RegExChar.ExtendedRegExChar	as ExtendedRegExChar
import			RegExChar.ExtendedRegExChar((+~), (=~))
import qualified	RegExDot.RegEx
import qualified	RegExDot.RegExOpts
import qualified	RegExDot.Result
import qualified	System.Exit
import qualified	System.IO

type LineNumber	= Int
type Matches	= Int

-- | Creates a 'RegExDot.RegExOpts.RegExOpts' record from 'CommandOptions.CommandOptions', and uses it to filter the lines read from the list of input-data files, the result of which is then printed.
grep
	:: CommandOptions.CommandOptions	-- ^ The match-criteria, which may include the name of the file on which to operate.
	-> [String]				-- ^ A supplementary list of files on which to operate
	-> IO ()				-- ^ Nothing is returned, since the result is printed.
grep commandOptions nonOptions
{-
 The algorithm depends fundamentally on whether the data comes from 'stdin' or from file.
 This is because of the requirement, in the former case, to deliver instantaneous feedback.
 This forces us to print matches as they're found, rather than batching them for return to the caller to print.
 If we COULD return 'IO [String]', then 'matchFilterStdIn' could be called instead of 'grep' when (fileName == "-"); annoyingly, it just doesn't work.
-}
	| any ($ fileNames) [null, all (== "-")]	= let
		matchFilterStdIn :: LineNumber -> Matches -> IO System.Exit.ExitCode
		matchFilterStdIn lineNumber matches	= {-#SCC "matchFilterStdIn" #-} do
			isEOF	<- System.IO.isEOF

			if isEOF
				then if listFilesWithMatches
					then return {-to IO-monad-} $ System.Exit.ExitFailure 1	-- No match has been found, otherwise we'd have exited earlier.
					else if listFilesWithoutMatch
						then putStrLn dummyFileName	>> return {-to IO-monad-} System.Exit.ExitSuccess
						else do
							Control.Monad.when countMatches $ print matches

							return {-to IO-monad-} $ if matches == 0
								then System.Exit.ExitFailure 1
								else System.Exit.ExitSuccess
				else {-not EOF-} do
					line	<- getLine	-- Process line-at-a-time, to provide instant feedback.

					let
						lineNumber' :: LineNumber
						lineNumber'	= succ lineNumber

						onSuccess :: String -> IO System.Exit.ExitCode
						onSuccess line'
							| listFilesWithMatches	= putStrLn dummyFileName	>> return {-to IO-monad-} System.Exit.ExitSuccess
							| listFilesWithoutMatch	= return {-to IO-monad-} $ System.Exit.ExitFailure 1
							| otherwise		= do
								Control.Monad.unless countMatches . putStrLn $ (
									if prependLineNumbers
										then showLineNumber lineNumber'
										else id
								 ) line'

								matchFilterStdIn lineNumber' $ succ matches	-- Recurse.

						onFailure, onMismatch :: IO System.Exit.ExitCode
						onFailure	= matchFilterStdIn lineNumber' matches	-- Recurse
						onMismatch
							| invertMatch	= onSuccess line
							| otherwise	= onFailure

					if verbose
						then let
							result :: RegExDot.RegEx.Result Char
							result	= line +~ regExOpts
						in if RegExDot.Result.isMatch result
							then if invertMatch
								then onFailure
								else onSuccess $ show result
							else onMismatch
						else {-terse-} if line =~ regExOpts
							then if invertMatch
								then onFailure
								else onSuccess line
							else onMismatch
			where
				dummyFileName :: String
				dummyFileName	= "<standard input>"
	in matchFilterStdIn 0 0 >>= System.Exit.exitWith
	| otherwise {-file-names provided-}	= let
		findMatch :: String -> RegExDot.RegEx.Result Char
		findMatch	= (+~ regExOpts)

		matchFilter :: [String] -> [String]
		matchFilter	= {-#SCC "matchFilter" #-} (
			if countMatches
				then return {-to List-monad-} . show . length	-- Substitute the list of matches, with its length.
				else id
		 ) . (
			let
				isMatch :: String -> Bool
				isMatch = {-#SCC "isMatch" #-} (if invertMatch then not else id) . (=~ regExOpts)
			in if prependLineNumbers
				then map (uncurry showLineNumber) . (
					if verbose
						then map (
							Control.Arrow.second show
						) . filter (
							RegExDot.Result.isMatch . snd
						) . map (
							Control.Arrow.second findMatch
						)
						else {-terse-} filter (isMatch . snd)
				) . zip [1 :: LineNumber ..]
				else {-unnumbered-} if verbose
					then map show . filter RegExDot.Result.isMatch . map findMatch
					else {-terse-} filter isMatch
		 )

		identifySource :: String -> [String] -> [String]
		identifySource fileName results
			| listFilesWithMatches			= [fileName | not isEmpty] {-list-comprehension-}
			| listFilesWithoutMatch			= [fileName | isEmpty] {-list-comprehension-}
			| length (Data.List.nub fileNames) > 1	= (showString fileName . separate) `map` results
			| otherwise				= results
			where
				isEmpty :: Bool
				isEmpty	= null results
	in do
		results <- (
			concat . Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rdeepseq (
				uncurry identifySource . Control.Arrow.second (matchFilter . {-#SCC "lines" #-} lines {-chops '\n'-})
			) . zip fileNames
		 ) `fmap` mapM readFile fileNames

		mapM_ putStrLn results

		System.Exit.exitWith $ if null results
			then System.Exit.ExitFailure 1
			else System.Exit.ExitSuccess
	where
		countMatches, invertMatch, prependLineNumbers, listFilesWithMatches, listFilesWithoutMatch, verbose :: Bool
		[countMatches, invertMatch, prependLineNumbers, listFilesWithMatches, listFilesWithoutMatch, verbose]	= ($ commandOptions) `map` [
			CommandOptions.countMatches,
			CommandOptions.invertMatch,
			CommandOptions.prependLineNumbers,
			CommandOptions.listFilesWithMatches,
			CommandOptions.listFilesWithoutMatch,
			CommandOptions.verbose
		 ] -- Divvy-up.

-- Separate the regEx & the names of the input-data files to feed it.
		extendedRegExChar	:: ExtendedRegExChar.ExtendedRegExChar
		fileNames		:: [String]
		(extendedRegExChar, fileNames)	= case CommandOptions.extendedRegExChar commandOptions of
			Just c	-> (c, nonOptions)
			_
				| null nonOptions	-> error "Undefined regex."
				| otherwise		-> read . head &&& tail $ nonOptions

-- Combine the regex & options, to create a complete task-description.
		regExOpts :: RegExDot.RegExOpts.RegExOpts ExtendedRegExChar.ExtendedRegExChar
		regExOpts	= RegExDot.RegExOpts.MkRegExOpts {
			RegExDot.RegExOpts.compilationOptions	= CommandOptions.compilationOptions commandOptions,
			RegExDot.RegExOpts.executionOptions	= CommandOptions.executionOptions commandOptions,
			RegExDot.RegExOpts.regEx		= extendedRegExChar
		}

-- Define functions used to format the output.
		separate :: ShowS
		separate	= showChar ':'

		showLineNumber :: LineNumber -> ShowS
		showLineNumber n	= shows n . separate

