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

	* Defines the options, read from the command-line,
	& as such assumes that the type-parameter of the underlying polymorphic /regex/-engine is 'Char'.

	* Supplies mutators, which ensure the consistency of the chosen set.
-}

module Grecce.CommandOptions(
-- * Types
-- ** Type-synonyms
	CompilationOptionsMutator,
	ExecutionOptionsMutator,
--	Transformer,
-- ** Data-types
	CommandOptions(..),
-- * Functions
-- ** Mutators
	setCompilationOption,
	setCountMatches,
	setExecutionOption,
	setInvertMatch,
	setPrependLineNumbers,
	setListFilesWithMatches,
	setListFilesWithoutMatch,
	setVerbose
) where

import qualified	RegExChar.ExtendedRegExChar	as ExtendedRegExChar
import qualified	RegExDot.CompilationOptions
import qualified	RegExDot.ExecutionOptions
import qualified	ToolShed.Defaultable
import qualified	ToolShed.Options

-- | The set of command-line options; based on those provided by /egrep/.
data CommandOptions	= MkCommandOptions {
	countMatches		:: Bool,						-- ^ Print only a count of matching lines per file.
	extendedRegExChar	:: Maybe ExtendedRegExChar.ExtendedRegExChar,		-- ^ The /regex/.
	invertMatch		:: Bool,						-- ^ Select non-matching lines.
	prependLineNumbers	:: Bool,						-- ^ Prepend the line-number of the matched input-data, to the output.
	listFilesWithMatches	:: Bool,						-- ^ List the names of any files containing at least one match.
	listFilesWithoutMatch	:: Bool,						-- ^ List the names of any files containing zero matches.
	verbose			:: Bool,						-- ^ Show the captured data.
	compilationOptions	:: RegExDot.CompilationOptions.CompilationOptions,	-- ^ Used to define the regex-request.
	executionOptions	:: RegExDot.ExecutionOptions.ExecutionOptions		-- ^ Used to control the regex-runtime.
} deriving Show

-- | An arbitrary function which transforms the supplied 'RegExDot.ExecutionOptions.ExecutionOptions'.
type Transformer	= CommandOptions -> CommandOptions

instance ToolShed.Defaultable.Defaultable CommandOptions	where
	defaultValue	= ToolShed.Options.blankValue {
		countMatches		= False,
		extendedRegExChar	= Nothing,
		invertMatch		= False,
		prependLineNumbers	= False,
		listFilesWithMatches	= False,
		listFilesWithoutMatch	= False,
		verbose			= False,
		compilationOptions	= ToolShed.Defaultable.defaultValue,
		executionOptions	= ToolShed.Defaultable.defaultValue
	}

instance ToolShed.Options.Options CommandOptions	where
	blankValue	= MkCommandOptions {
		countMatches		= undefined,
		extendedRegExChar	= undefined,
		invertMatch		= undefined,
		prependLineNumbers	= undefined,
		listFilesWithMatches	= undefined,
		listFilesWithoutMatch	= undefined,
		verbose			= undefined,
		compilationOptions	= undefined,
		executionOptions	= undefined
	}

-- | Mutator.
setCountMatches :: Transformer
setCountMatches commandOptions
	| ($ commandOptions) `any` [
		prependLineNumbers,
		listFilesWithoutMatch,
		listFilesWithMatches,
		verbose
	]		= error "Grecce.CommandOptions.setCountMatches:\t'countMatches' is incompatible with; 'prependLineNumbers', 'listFilesWithoutMatch', 'listFilesWithMatches' & 'verbose'."
	| otherwise	= commandOptions { countMatches = True }

-- | Mutator.
setInvertMatch :: Transformer
setInvertMatch commandOptions
	| verbose commandOptions	= error "Grecce.CommandOptions.setInvertMatch:\t'invertMatch' is incompatible with 'verbose'."
	| otherwise			= commandOptions { invertMatch = True }

-- | Mutator.
setPrependLineNumbers :: Transformer
setPrependLineNumbers commandOptions
	| ($ commandOptions) `any` [
		countMatches,
		listFilesWithoutMatch,
		listFilesWithMatches
	]		= error "Grecce.CommandOptions.setPrependLineNumbers:\t'prependLineNumbers' is incompatible with 'countMatches', 'listFilesWithoutMatch' & 'listFilesWithMatches'."
	| otherwise	= commandOptions { prependLineNumbers = True }

-- | Mutator.
setListFilesWithMatches :: Transformer
setListFilesWithMatches commandOptions
	| ($ commandOptions) `any` [
		countMatches,
		prependLineNumbers,
		listFilesWithoutMatch,
		verbose
	]		= error "Grecce.CommandOptions.setListFilesWithMatches:\t'listFilesWithMatches' is incompatible with; 'countMatches', 'prependLineNumbers', 'listFilesWithoutMatch' & 'verbose'."
	| otherwise	= commandOptions { listFilesWithMatches = True }

-- | Mutator.
setListFilesWithoutMatch :: Transformer
setListFilesWithoutMatch commandOptions
	| ($ commandOptions) `any` [
		countMatches,
		prependLineNumbers,
		listFilesWithMatches,
		verbose
	]		= error "Grecce.CommandOptions.setListFilesWithoutMatch:\t'listFilesWithoutMatch' is incompatible with; 'countMatches', 'prependLineNumbers', 'listFilesWithMatches' & 'verbose'."
	| otherwise	= commandOptions { listFilesWithoutMatch = True }

-- | Mutator, which additionally sets the corresponding 'RegExDot.ExecutionOptions.ExecutionOptions'.
setVerbose :: Transformer
setVerbose commandOptions
	| ($ commandOptions) `any` [
		countMatches,
		invertMatch,
		listFilesWithoutMatch,
		listFilesWithMatches
	]		= error "Grecce.CommandOptions.setVerbose:\t'verbose' is incompatible with; 'countMatches', 'invertMatch', 'listFilesWithoutMatch' & 'listFilesWithMatches'."
	| otherwise	= commandOptions {
		verbose			= True,
		executionOptions	= RegExDot.ExecutionOptions.setVerbose True $ executionOptions commandOptions
	}

-- | A function which accepts a 'String' & the 'RegExDot.CompilationOptions.CompilationOptions' to transform, using its value.
type CompilationOptionsMutator	= String -> RegExDot.CompilationOptions.CompilationOptions -> RegExDot.CompilationOptions.CompilationOptions

-- | Generic mutator, which accepts a function & the parameter-string it requires, to amend an unspecified field of 'RegExDot.CompilationOptions.CompilationOptions'.
setCompilationOption :: CompilationOptionsMutator -> String -> Transformer
setCompilationOption compilationOptionsMutator s commandOptions	= commandOptions { compilationOptions = compilationOptionsMutator s $ compilationOptions commandOptions }

-- | A function which accepts a 'String' & the 'RegExDot.ExecutionOptions.ExecutionOptions' to transform, using its value.
type ExecutionOptionsMutator	= String -> RegExDot.ExecutionOptions.ExecutionOptions -> RegExDot.ExecutionOptions.ExecutionOptions

-- | Generic mutator, which accepts a function & the parameter-string it requires, to amend an unspecified field of 'RegExDot.ExecutionOptions.ExecutionOptions'.
setExecutionOption :: ExecutionOptionsMutator -> String -> Transformer
setExecutionOption executionOptionsMutator s commandOptions	= commandOptions { executionOptions = executionOptionsMutator s $ executionOptions commandOptions }

