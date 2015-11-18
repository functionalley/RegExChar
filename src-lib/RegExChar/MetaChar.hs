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
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Implements 'RegExDot.Meta.ShortcutExpander' 'Char', to implement some of the traditional /Perl-style shortcuts/:

	[@\\d@]		=> Any 'Data.Char.isDigit'-character.

	[@\\D@]		=> Any non-'Data.Char.isDigit' character.

	[@\\s@]		=> Any 'Data.Char.isSpace'-character.

	[@\\S@]		=> Any non-'Data.Char.isSpace' character.

	[@\\w@]		=> Any 'Data.Char.isWord' letter.

	[@\\W@]		=> Any non-'Data.Char.isWord' letter.

	* Enables one to compose concise regexen, containing any 'Char' that's a member of one of these predefined sets.

	* Though 'RegExDot.Meta.Meta' is polymorphic, & the type-parameter can't be assumed to implement either 'Enum' or 'Ord',
	'Char' actually does, so this module is able to implement ranges within a /Bracket-expression/.

	* Defines specialised instances of 'Read' & 'Show', to cope with /Perl-style shortcuts/, /Posix Character-classes/ & /Bracket-expression/ range-specifications.

 [@TODO@]	Use @Environment.getLocale@. Regrettably, this returns in the "IO"-monad, & even it didn't, how does one pass that information to 'Read' ?!
-}

module RegExChar.MetaChar(
-- * Types
--	AssociationList,
--	Dictionary,
-- ** Type-synonyms
	MetaChar(..),
-- * Constants
--	bracketExpressionRangeToken,
--	posixCharacterClassDelimiters,
-- * Functions
--	implementPerlShortcut,
	metaCharParser,
-- ** Accessors (Deconstructors)
	deconstruct
) where

import			Control.Arrow((***))
import qualified	Data.Char
import qualified	Data.Map
import qualified	RegExDot.BracketExpression
import qualified	RegExDot.BracketExpressionMember
import qualified	RegExDot.Consumer
import qualified	RegExDot.Meta
import qualified	RegExDot.RegEx
import qualified	RegExDot.Repeatable
import qualified	RegExDot.ShowablePredicate
import qualified	Text.ParserCombinators.Parsec	as Parsec
import			Text.ParserCombinators.Parsec((<?>))
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.SelfValidate

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

-- | Holds a mapping from an escape-sequence & the predicate via which it is implemented.
type AssociationList key	= [(key, RegExDot.ShowablePredicate.Predicate Char)]

-- | Holds a mapping from an escape-sequence & the predicate via which it is implemented.
type Dictionary key		= Data.Map.Map key (RegExDot.ShowablePredicate.Predicate Char)

instance RegExDot.BracketExpressionMember.ShortcutExpander Char	where
	findPredicate shortcut	= RegExDot.ShowablePredicate.MkShowablePredicate [RegExDot.Meta.shortcutToken, shortcut] <$> shortcut `Data.Map.lookup` dictionary where
		dictionary :: Dictionary Char
		dictionary	= Data.Map.fromList $ perlShortcuts ++ invert perlShortcuts	where
			perlShortcuts :: AssociationList Char
			perlShortcuts	= [
				('d',	Data.Char.isDigit),
				('s',	Data.Char.isSpace),
				('w',	\c -> ($ c) `any` [Data.Char.isAlphaNum, (== '_')])
			 ]

			invert :: AssociationList Char -> AssociationList Char
			invert	= map (Data.Char.toUpper *** (not .))

instance RegExDot.Meta.ShortcutExpander Char	where
	expand c	= case RegExDot.BracketExpressionMember.findPredicate c of
		Just showablePredicate	-> RegExDot.Meta.Predicate showablePredicate
		_			-> RegExDot.Meta.Literal c

-- | A specialised instance, encapsulated to permit tailored instance-declarations.
newtype MetaChar	= MkMetaChar (RegExDot.Meta.Meta Char)	deriving Eq

-- | Accessor.
deconstruct :: MetaChar -> RegExDot.Meta.Meta Char
deconstruct (MkMetaChar metaChar)	= metaChar

instance ToolShed.SelfValidate.SelfValidator MetaChar	where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstruct

instance RegExDot.Consumer.Consumer MetaChar	where
	consumptionProfile	= RegExDot.Consumer.consumptionProfile . deconstruct
	starHeight		= RegExDot.Consumer.starHeight . deconstruct

-- | Builds a parser of traditional regex-syntax, which understands 'Char'-specific concepts like /Perl-style shortcuts/ & /Posix Character-classes/.
metaCharParser :: Parsec.Parser MetaChar
metaCharParser	= MkMetaChar <$> Parsec.choice [
	(Parsec.char RegExDot.Meta.anyToken <?> "RegExDot.Meta.anyToken " ++ show RegExDot.Meta.anyToken) >> return {-to ParsecT-monad-} RegExDot.Meta.Any,
	(Parsec.char RegExDot.Meta.shortcutToken <?> "RegExDot.Meta.shortcutToken " ++ show RegExDot.Meta.shortcutToken) >> RegExDot.Meta.expand <$> Parsec.anyChar,
	uncurry Parsec.between (ToolShed.Data.Pair.mirror Parsec.char RegExDot.BracketExpression.delimiterTokens) (
		do
			let
				implementPosixCharacterClass :: String -> Maybe (RegExDot.ShowablePredicate.ShowablePredicate Char)
				implementPosixCharacterClass identifier	= RegExDot.ShowablePredicate.MkShowablePredicate (
					fst posixCharacterClassDelimiters ++ identifier ++ snd posixCharacterClassDelimiters
				 ) <$> identifier `Data.Map.lookup` dictionary	where
					dictionary :: Dictionary String
					dictionary	= Data.Map.fromList $ posixCharacterClasses ++ invert posixCharacterClasses	where
						posixCharacterClasses :: AssociationList String
						posixCharacterClasses	= [
							("alnum",	Data.Char.isAlphaNum),
							("alpha",	Data.Char.isAlpha),
							("ascii",	Data.Char.isAscii),
							("blank",	(`elem` " \t")),
							("cntrl",	Data.Char.isControl),
							("digit",	Data.Char.isDigit),
							("graph",	\c	-> not $ ($ c) `any` [Data.Char.isSpace, Data.Char.isControl]),
							("lower",	Data.Char.isLower),
							("print",	Data.Char.isPrint),
							("punct",	Data.Char.isPunctuation),
							("space",	Data.Char.isSpace),
							("upper",	Data.Char.isUpper),
							("word",	\c	-> ($ c) `any` [Data.Char.isAlphaNum, (== '_')]),
							("xdigit",	Data.Char.isHexDigit)
						 ]

						invert :: AssociationList String -> AssociationList String
						invert	= map $ (RegExDot.BracketExpression.negationToken :) *** (not .)

			cTor	<- Parsec.option RegExDot.Meta.AnyOf {-default-} $ (Parsec.char RegExDot.BracketExpression.negationToken <?> "RegExDot.BracketExpression.negationToken " ++ show RegExDot.BracketExpression.negationToken) >> return {-to ParsecT-monad-} RegExDot.Meta.NoneOf

			literalBracketExpressionTerminator	<- Parsec.option [] {-default-} $ return {-to List-monad-} . RegExDot.BracketExpressionMember.Literal <$> (
				Parsec.char (snd RegExDot.BracketExpression.delimiterTokens)	<?> "Literal Bracket-expression terminator " ++ show (snd RegExDot.BracketExpression.delimiterTokens)
			 ) -- If the first item in a BracketExpression (or negated BracketExpression) is the terminator-token, then it is treated as a 'RegExDot.BracketExpressionMember.Literal'.

			cTor . (literalBracketExpressionTerminator ++) <$> Parsec.many {-potentially zero-} (
				Parsec.choice [
					(
						do
							_	<- Parsec.char RegExDot.Meta.shortcutToken	<?> "RegExDot.Meta.shortcutToken " ++ show RegExDot.Meta.shortcutToken
							c	<- Parsec.anyChar

							return {-to ParsecT-monad-} $ case RegExDot.BracketExpressionMember.findPredicate c of
								Just showablePredicate	-> RegExDot.BracketExpressionMember.Predicate showablePredicate
								_			-> RegExDot.BracketExpressionMember.Literal c	-- Escaped literal.
					) <?> "Perl-style shortcut",
					Parsec.try (
						uncurry Parsec.between (ToolShed.Data.Pair.mirror Parsec.string posixCharacterClassDelimiters) (
							do
								identifier	<- Parsec.many1 $ Parsec.noneOf [head $ snd posixCharacterClassDelimiters]

								case implementPosixCharacterClass identifier of
									Just showablePredicate	-> return {-to ParsecT-monad-} $ RegExDot.BracketExpressionMember.Predicate showablePredicate
									_			-> Parsec.unexpected $ "MetaChar.metaCharParser:\tunrecognised Posix Character-class; " ++ show identifier
						) <?> "Posix Character-class " ++ show posixCharacterClassDelimiters
					), -- Regurgitate erroneously consumed input.
					Parsec.try (
						(
							do
								rangeStart	<- Parsec.noneOf [snd RegExDot.BracketExpression.delimiterTokens]
								_		<- Parsec.char bracketExpressionRangeToken	<?> "bracketExpressionRangeToken " ++ show bracketExpressionRangeToken
								rangeEnd	<- Parsec.noneOf [snd RegExDot.BracketExpression.delimiterTokens]

								return {-to ParsecT-monad-} . RegExDot.BracketExpressionMember.Predicate . RegExDot.ShowablePredicate.MkShowablePredicate [
									rangeStart,
									bracketExpressionRangeToken,
									rangeEnd
								 ] $ \c -> rangeStart <= c && c <= rangeEnd	-- Create custom predicate, utilising "Ord Char".
						) <?> "Bracket-expression range"
					), -- Regurgitate erroneously consumed input.
					RegExDot.BracketExpressionMember.Literal <$> Parsec.noneOf [snd RegExDot.BracketExpression.delimiterTokens]	<?> "RegExDot.BracketExpressionMember.Literal"	-- TODO: the first Char-member can be a literal ']'.
				] <?> "Bracket-expression member"
			 ) <?> "Bracket-expression member-list"
	) <?> "RegExDot.BracketExpression.delimiterTokens " ++ show RegExDot.BracketExpression.delimiterTokens,
	RegExDot.Meta.Literal <$> Parsec.noneOf RegExDot.RegEx.tokens
 ]

instance Read MetaChar	where
	readsPrec _	= (
		(error . showString "RegExChar.MetaChar.readsPrec:\tparse-error; " . show) `either` return
	 ) . Parsec.parse (
		(,) <$> metaCharParser <*> Parsec.getInput
	 ) "MetaChar"

-- | The token used to signify an ordered range of members in a /Bracket-expression/.
bracketExpressionRangeToken :: Char
bracketExpressionRangeToken	= '-'

-- | The delimiters of a /Posix Character-class/.
posixCharacterClassDelimiters :: (String, String)
posixCharacterClassDelimiters	= ("[:", ":]")

instance Show MetaChar	where
	showsPrec _ (MkMetaChar RegExDot.Meta.Any)				= showChar RegExDot.Meta.anyToken
	showsPrec _ (MkMetaChar (RegExDot.Meta.Literal c))			= (
		if c `elem` [
			fst RegExDot.BracketExpression.delimiterTokens,
			fst RegExDot.Repeatable.rangeDelimiters,
			RegExDot.Repeatable.oneOrMoreToken,
			RegExDot.Repeatable.zeroOrMoreToken,
			RegExDot.Repeatable.zeroOrOneToken,
			RegExDot.Meta.anyToken,
			RegExDot.Meta.shortcutToken
		] ++ RegExDot.RegEx.tokens
			then showChar RegExDot.Meta.shortcutToken
			else id
	 ) . showChar c
	showsPrec _ (MkMetaChar (RegExDot.Meta.AnyOf bracketExpression))	= showChar (
		fst RegExDot.BracketExpression.delimiterTokens
	 ) . showString (
		foldr (
			\e	-> case e of
				RegExDot.BracketExpressionMember.Predicate showablePredicate	-> shows showablePredicate
				RegExDot.BracketExpressionMember.Literal literal		-> (
					if literal `elem` [
						bracketExpressionRangeToken,		-- CAVEAT: only unambiguously literal when at the start or end of a "BracketExpression".
						RegExDot.Meta.shortcutToken,
						snd RegExDot.BracketExpression.delimiterTokens	-- CAVEAT: only unambiguously literal when at the start of a "BracketExpression".
					]
						then showChar RegExDot.Meta.shortcutToken
						else id
				 ) . showChar literal
		) (
			showChar (snd RegExDot.BracketExpression.delimiterTokens) ""	-- Initial value.
		) bracketExpression
	 )
	showsPrec _ (MkMetaChar (RegExDot.Meta.NoneOf bracketExpression))	= showChar x . showChar RegExDot.BracketExpression.negationToken . showString xs	where (x : xs)	= show . MkMetaChar $ RegExDot.Meta.AnyOf bracketExpression
	showsPrec _ (MkMetaChar m)						= shows m

