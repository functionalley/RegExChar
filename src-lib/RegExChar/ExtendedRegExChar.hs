{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	An 'RegExDot.RegEx.ExtendedRegEx', which has been specialised for 'Char', to create a tradition non-polymorphic /regex/.
-}

module RegExChar.ExtendedRegExChar(
-- * Types
-- ** Type-synonyms
	ExtendedRegExChar(..),
	InputData,
-- * Functions
-- ** Operators
	(+~),
	(=~),
	(/~)
) where

import qualified	Data.List
import qualified	RegExChar.MetaChar		as MetaChar
import qualified	RegExDot.Anchor
import qualified	RegExDot.Consumer
import			RegExDot.DSL((<~>), (-:))
import qualified	RegExDot.RegEx
import qualified	RegExDot.RegExOpts
import qualified	RegExDot.Repeatable
import qualified	Text.ParserCombinators.Parsec	as Parsec
import			Text.ParserCombinators.Parsec((<?>), (<|>))
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.SelfValidate

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

infix 4 +~, =~, /~	-- Same as (==) & (/=).

-- | Specialise a 'RegExDot.RegEx.ExtendedRegEx' for 'Char', & encapsulate it to permit tailored instance-declarations.
data ExtendedRegExChar	= MkExtendedRegExChar {
	hasNonCapturingTopLevelAlternatives	:: Bool,	-- ^ The string from which a 'RegExDot.RegEx.ExtendedRegEx' is read, may, if data-capture isn't required, omit explicit delimiters around top-level 'RegExDot.RegEx.Alternatives'.
	extendedRegEx				:: RegExDot.RegEx.ExtendedRegEx Char
} deriving Eq

-- | Abbreviation.
type InputData	= RegExDot.RegEx.InputData Char

instance ToolShed.SelfValidate.SelfValidator ExtendedRegExChar	where
	getErrors MkExtendedRegExChar {
		hasNonCapturingTopLevelAlternatives	= hasNonCapturingTopLevelAlternatives',
		extendedRegEx				= extendedRegEx'
	}
		| not $ ToolShed.SelfValidate.isValid extendedRegEx'	= ToolShed.SelfValidate.getErrors extendedRegEx'
		| otherwise						= ToolShed.SelfValidate.extractErrors [
			(hasNonCapturingTopLevelAlternatives' && any (not . RegExDot.RegEx.isCaptureGroup . RegExDot.Repeatable.base) (RegExDot.RegEx.concatenation extendedRegEx'), "Invalid NonCapturingTopLevelAlternatives.")
		]

instance RegExDot.Consumer.Consumer ExtendedRegExChar	where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . extendedRegEx
	starHeight		= RegExDot.Consumer.starHeight . extendedRegEx

instance RegExDot.RegEx.ShortcutExpander Char	where
	expand c	= error $ "RegExDot.RegEx.ShortcutExpander.expand RegExChar.ExtendedRegExChar:\tunrecognised shortcut '" ++ show c ++ "'."

instance Read ExtendedRegExChar	where
	readsPrec _ s
		| s == reverse RegExDot.Anchor.tokens	= [
			(
				MkExtendedRegExChar {
					hasNonCapturingTopLevelAlternatives	= False,
					extendedRegEx				= (Just RegExDot.Anchor.Bow, Just RegExDot.Anchor.Stern) <~> []
				},
				""
			)
		] -- The order of adjacent zero-width assertions is irrelevant.
		| otherwise			= let
			extendedRegExCharParser :: Parsec.Parser ExtendedRegExChar
			extendedRegExCharParser	= reduce {-correct prior assumption-} <$> alternativesParser {-assume non-capturing top-level Alternatives-} where
				reduce :: RegExDot.RegEx.Alternatives Char -> ExtendedRegExChar
				reduce alternatives
					| RegExDot.RegEx.isSingletonAlternatives alternatives	= MkExtendedRegExChar False . head $ RegExDot.RegEx.deconstructAlternatives alternatives
					| otherwise						= MkExtendedRegExChar True $ RegExDot.Anchor.unanchored <~> RegExDot.RegEx.CaptureGroup alternatives -: []	-- Infer non-capturing top-level 'RegExDot.RegEx.Alternatives' from the presence of 'RegExDot.RegEx.alternativeExtendedRegExSeparatorToken's.

				alternativesParser :: Parsec.Parser (RegExDot.RegEx.Alternatives Char)
				alternativesParser	= RegExDot.RegEx.MkAlternatives <$> extendedRegExParser `Parsec.sepBy1` (Parsec.char RegExDot.RegEx.alternativeExtendedRegExSeparatorToken <?> "RegExDot.RegEx.alternativeExtendedRegExSeparatorToken " ++ show RegExDot.RegEx.alternativeExtendedRegExSeparatorToken)	where
					extendedRegExParser :: Parsec.Parser (RegExDot.RegEx.ExtendedRegEx Char)
					extendedRegExParser	= do
						maybeBowAnchor			<- Parsec.option Nothing {-default-} $ (Parsec.char RegExDot.Anchor.bowToken <?> "RegExDot.Anchor.bowToken " ++ show RegExDot.Anchor.bowToken) >> return {-to ParsecT-monad-} (Just RegExDot.Anchor.Bow)
						repeatableRequirementList	<- repeatableRequirementListParser

						(
							do
								repeatableCaptureGroup	<- RegExDot.Repeatable.repeatableParser . RegExDot.RegEx.CaptureGroup =<< uncurry Parsec.between (
									ToolShed.Data.Pair.mirror Parsec.char RegExDot.RegEx.captureGroupDelimiters
								 ) alternativesParser {-recurse-} <?> "RegExDot.RegEx.captureGroupDelimiters " ++ show RegExDot.RegEx.captureGroupDelimiters

								extendedRegEx'		<- extendedRegExParser	-- Recurse.

								return {-to ParsecT-monad-} $ RegExDot.RegEx.transformExtendedRegEx ((repeatableRequirementList ++) . (repeatableCaptureGroup :)) extendedRegEx' { RegExDot.RegEx.bowAnchor = maybeBowAnchor }
						 ) <|> (
							do
								maybeSternAnchor	<- Parsec.option Nothing {-default-} $ (Parsec.char RegExDot.Anchor.sternToken <?> "RegExDot.Anchor.sternToken " ++ show RegExDot.Anchor.sternToken) >> return {-to ParsecT-monad-} (Just RegExDot.Anchor.Stern)

								return {-to ParsecT-monad-} RegExDot.RegEx.MkExtendedRegEx {
									RegExDot.RegEx.bowAnchor	= maybeBowAnchor,
									RegExDot.RegEx.concatenation	= repeatableRequirementList,
									RegExDot.RegEx.sternAnchor	= maybeSternAnchor
								}
						 )
						where
							repeatableRequirementListParser :: Parsec.Parser (RegExDot.RegEx.Concatenation Char)
							repeatableRequirementListParser	= Parsec.choice [
								Parsec.try . Parsec.lookAhead $ (
									Parsec.char RegExDot.Anchor.sternToken <?> "RegExDot.Anchor.sternToken " ++ show RegExDot.Anchor.sternToken
								) >> (
									(
										Parsec.eof >> return {-to ParsecT-monad-} []
									) <|> (
										Parsec.oneOf [RegExDot.RegEx.alternativeExtendedRegExSeparatorToken, snd RegExDot.RegEx.captureGroupDelimiters] >> return {-to ParsecT-monad-} []
									)
								),
								(:) <$> (
									MetaChar.metaCharParser >>= RegExDot.Repeatable.repeatableParser . RegExDot.RegEx.Require . MetaChar.deconstruct
								) <*> repeatableRequirementListParser, {-recurse-}
								return {-to ParsecT-monad-} []
							 ]

		in const [] `either` (
			\pair@(extendedRegExChar, _)	-> [pair | ToolShed.SelfValidate.isValid extendedRegExChar]
		) $ Parsec.parse (
			(,) <$> extendedRegExCharParser <*> Parsec.getInput
		) "ExtendedRegExChar" s

instance Show ExtendedRegExChar	where
	showsPrec _ MkExtendedRegExChar {
		hasNonCapturingTopLevelAlternatives	= hasNonCapturingTopLevelAlternatives',
		extendedRegEx				= RegExDot.RegEx.MkExtendedRegEx {
			RegExDot.RegEx.bowAnchor	= maybeBowAnchor,
			RegExDot.RegEx.concatenation	= concatenation',
			RegExDot.RegEx.sternAnchor	= maybeSternAnchor
		}
	} = RegExDot.RegEx.showsMaybeAnchor maybeBowAnchor . foldl (.) id (
		(
			let
				showAlternatives :: RegExDot.RegEx.Alternatives Char -> [ShowS]
				showAlternatives	= Data.List.intersperse (showChar RegExDot.RegEx.alternativeExtendedRegExSeparatorToken) . map (shows . MkExtendedRegExChar False) . RegExDot.RegEx.deconstructAlternatives
			in if hasNonCapturingTopLevelAlternatives'
				then map (
					\repeatablePattern	-> case RegExDot.Repeatable.base repeatablePattern of
						RegExDot.RegEx.CaptureGroup alternatives	-> foldr (.) id $ showAlternatives alternatives
						_						-> error $ "Show RegExChar.ExtendedRegExChar: unexpected " ++ show repeatablePattern
				)
				else map (
					\repeatablePattern	-> (
						case RegExDot.Repeatable.base repeatablePattern of
							RegExDot.RegEx.Require meta			-> shows $ MetaChar.MkMetaChar meta
							RegExDot.RegEx.CaptureGroup alternatives	-> showChar (
								fst RegExDot.RegEx.captureGroupDelimiters
							 ) . foldr (.) (
								showChar $ snd RegExDot.RegEx.captureGroupDelimiters	-- Initial value.
							 ) (
								showAlternatives alternatives
							 )
					) . RegExDot.Repeatable.showSuffix repeatablePattern
				)
		 ) concatenation'
	 ) . RegExDot.RegEx.showsMaybeAnchor maybeSternAnchor

-- | A veneer over the underlying polymorphic operator, 'RegExDot.RegEx.+~'.
(+~)
	:: InputData						-- ^ The input-data string.
	-> RegExDot.RegExOpts.RegExOpts ExtendedRegExChar	-- ^ The match-options, parameterised by the regex-specification.
	-> RegExDot.RegEx.Result Char
inputData +~ regExOpts	= inputData RegExDot.RegEx.+~ fmap extendedRegEx regExOpts	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.

-- | A veneer over the underlying polymorphic operator, 'RegExDot.RegEx.=~'.
(=~)
	:: InputData						-- ^ The input-data string.
	-> RegExDot.RegExOpts.RegExOpts ExtendedRegExChar	-- ^ The match-options, parameterised by the regex-specification.
	-> Bool
inputData =~ regExOpts	= inputData RegExDot.RegEx.=~ fmap extendedRegEx regExOpts	-- CAVEAT: <https://hackage.haskell.org/trac/haskell-prime/wiki/QualifiedOperators>.

-- | Pattern-mismatch operator.
(/~)
	:: InputData						-- ^ The input-data string.
	-> RegExDot.RegExOpts.RegExOpts ExtendedRegExChar	-- ^ The match-options, parameterised by the regex-specification.
	-> Bool
(/~) inputData	= not . (inputData =~)

