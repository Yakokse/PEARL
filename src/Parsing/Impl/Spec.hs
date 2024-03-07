module Parsing.Impl.Spec where

import Utils.Maps
import Utils.Error

import RL.Values

import Parsing.Impl.Common

import Text.Parsec

-- parse a string as a specialization specification
parseSpec :: String -> EM Store
parseSpec = parseStr pFile
  where pFile = fromList <$> (whitespace *> many pDeclaration)

-- parse a single declaration in a specification
pDeclaration :: Parser (Name, Value)
pDeclaration = (,) <$> (pName <* symbol "=") <*> pConstant
