{-
 - Copyright (C) 2009-2010 Vincent Hanquez <vincent@snarc.org>
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU Lesser General Public License as published by
 - the Free Software Foundation; version 2.1 only.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -}

{- |
   Module      : Text.SimpleTemplate
   Copyright   : Copyright (C) 2009-2010 Vincent Hanquez
   License     : BSD3
   Maintainer  : Vincent Hanquez <vincent@snarc.org>
   Stability   : alpha
   Portabily   : haven't tested
-}

module Text.SimpleTemplate (
	TGroup,
	Template,
	TAtom(..),
	getVariables,
	parseTemplate,
	parseTGroup,
	renderTemplate,
	marshallTemplate,
	unmarshallTemplate,
	marshallTGroup,
	unmarshallTGroup
	) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isDigit, isAlpha)
import Data.Maybe (catMaybes)
import qualified Data.Map as M

data TAtom = Text ByteString | Var ByteString deriving (Show)
type Template = [TAtom]
type TGroup = M.Map ByteString Template

instance Binary TAtom where
	put (Text s) = putWord8 0 >> put s
	put (Var s)  = putWord8 1 >> put s
	get = do
		tag <- getWord8
		case tag of
			0 -> get >>= \a -> return (Text a)
			1 -> get >>= \a -> return (Var a)
			_ -> error "invalid tag"

tailSafe :: ByteString -> ByteString
tailSafe s
	| B.null s  = B.empty
	| otherwise = B.tail s

isVariableChar :: Char -> Bool
isVariableChar c = isAlpha c || isDigit c

isVariable :: ByteString -> Bool
isVariable = and . map isVariableChar . B.unpack

getVariable :: TAtom -> Maybe ByteString
getVariable (Var s) = Just s
getVariable _       = Nothing

parseVar :: ByteString -> Template
parseVar s
	| B.null s  = []
	| otherwise =
		let (b, a) = B.break ((==) '$') s in
		if isVariable b
			then Var b  : (parseText $ tailSafe a)
			else Text b : (parseVar $ tailSafe a)

parseText :: ByteString -> Template
parseText s
	| B.null s  = []
	| otherwise = Text b : (parseVar $ tailSafe a)
		where
			(b, a) = B.break ((==) '$') s

{- parse a byteString to a template -}
parseTemplate :: ByteString -> Template
parseTemplate s
	| B.null s        = []
	| B.head s == '$' = parseVar $ B.tail s
	| otherwise       = parseText s

parseTGroupHelper :: ByteString -> [ByteString] -> [ByteString] -> [(ByteString, Template)]
parseTGroupHelper cat accu []          = [ (cat, parseTemplate $ B.unlines $ reverse accu) ]
parseTGroupHelper cat accu (line : ls) =
	if B.length line >= 2 && B.head line == '[' && B.last line == ']'
		then
			let newcat = B.init $ B.tail line in
			if cat == B.empty
				then parseTGroupHelper newcat [] ls
				else (cat, parseTemplate $ B.unlines $ reverse accu) : parseTGroupHelper newcat [] ls
		else
			parseTGroupHelper cat (line : accu) ls

{- parse a byteString to a tgroup
 - FIXME: this isn't really nice. probably move this to a more streamish computation -}
parseTGroup :: ByteString -> TGroup
parseTGroup content = M.fromList $ parseTGroupHelper B.empty [] $ B.lines content

{- get all variables : mainly for debugging purpose -}
getVariables :: Template -> [ByteString]
getVariables = catMaybes . map getVariable

{- render one Atom -}
renderAtom :: M.Map ByteString ByteString -> TAtom -> ByteString
renderAtom _     (Text b) = b
renderAtom attrs (Var s)  = M.findWithDefault B.empty s attrs

{- take a template and attributes and create a bytestring where all template variables
 - has been replaced by the attributes -}
renderTemplate :: Template -> M.Map ByteString ByteString -> ByteString
renderTemplate template attrs =
	B.concat $ map (renderAtom attrs) template

{- marshall a template to a binary bytestring -}
marshallTemplate :: Template -> ByteString
marshallTemplate = encode . map encode

{- unmarshall a template from a binary bytestring -}
unmarshallTemplate :: ByteString -> Template
unmarshallTemplate = map decode . decode

{- marshall a tgroup to a binary bytestring -}
marshallTGroup :: TGroup -> ByteString
marshallTGroup = encode . map encode . M.toList

{- unmarshall a tgroup from a binary bytestring -}
unmarshallTGroup :: ByteString -> TGroup
unmarshallTGroup = M.fromList . map decode . decode
