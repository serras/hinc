{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
module Hinc.Parser where

import           Control.Applicative          (Alternative)
import           Data.List                    (intercalate)
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Data.Void
import           Language.Haskell.Exts.Pretty (Pretty, prettyPrint)
import qualified Language.Haskell.Exts.Syntax as Hs
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer   as L

type Parser = Parsec Void T.Text

tester :: Pretty a => Parser a -> T.Text -> IO ()
tester p txt = case prettyPrint <$> parse p "test" txt of
                 Left  e -> print e
                 Right s -> putStrLn s

-- BASIC PARSERS
-- =============

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme spaceConsumer

parens, braces, angles, brackets :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
angles   = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")

semicolon, comma, colon, dot, arrow :: Parser T.Text
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
arrow     = symbol "=>"

lower, upper, anyChar :: Parser Char
lower   = lowerChar <|> single '_'
upper   = upperChar
anyChar = alphaNumChar <|> single '_'

optionalOrEmpty :: Alternative f => f [a] -> f [a]
optionalOrEmpty p
  = fromMaybe [] <$> optional p

-- NAMES
-- =====

varP, litP, nameP :: Parser (Hs.Name ())
varP = Hs.Ident () <$> lexeme ((:) <$> lower <*> many anyChar)
litP = Hs.Ident () <$> lexeme ((:) <$> upper <*> many anyChar)
nameP = varP <|> litP

modNameP :: Parser (Hs.ModuleName ())
modNameP
  = Hs.ModuleName () . intercalate "."
      <$> ((\(Hs.Ident _ i) -> i) <$> litP) `sepBy1` single '.'

qnameP :: Parser (Hs.Name ()) -> Parser (Hs.QName ())
qnameP r
  =   Hs.Qual   () <$> brackets modNameP <*> r
  <|> Hs.UnQual () <$> r

-- EXPRESSIONS
-- ===========

argP :: Parser (Hs.Name (), Maybe (Hs.Type ()))
argP = (,) <$> varP <*> optional (colon >> typeP)

argPPat :: Parser (Hs.Pat ())
argPPat = f <$> argP
  where
    f (nm, Nothing) = Hs.PVar () nm
    f (nm, Just ty) = Hs.PatTypeSig () (Hs.PVar () nm) ty

-- variable, constant, or expression between parentheses
basicExprP :: Parser (Hs.Exp ())
basicExprP
  =   try (parens exprP)
  <|> Hs.Var () <$> qnameP varP
  <|> Hs.Con () <$> qnameP litP

-- basic expression followed by arguments
exprWithArgsP :: Parser (Hs.Exp ())
exprWithArgsP
  = foldl (Hs.App ())
      <$> basicExprP
      <*> optionalOrEmpty (parens (exprP `sepBy` comma))

-- f1(...).f2(...).f3(...) and so on
dottedExprP :: Parser (Hs.Exp ())
dottedExprP = f . reverse <$> dottedExprP'
  where
    f [x]    = x
    f (x:xs) = Hs.App () x (f xs)
    dottedExprP' :: Parser [Hs.Exp ()]
    dottedExprP' = (:) <$> exprWithArgsP
                       <*> (try (dot *> dottedExprP')
                           <|> pure [])

exprP :: Parser (Hs.Exp ())
exprP
  =   Hs.If () <$ symbol "if" <*> parens exprP
               <*> exprP <* symbol "else" <*> exprP
  <|> Hs.Do () <$ symbol "effect"
               <*> braces (many stmtP)
  <|> braces (Hs.Let () <$> bindsP <*> exprP)
  <|> brackets (Hs.List () <$> exprP `sepBy` comma)
  <|> try (Hs.Lambda ()
             <$> parens (argPPat `sepBy` comma)
             <*  arrow
             <*> exprP)
  <|> Hs.Lit () <$> literalP
  <|> dottedExprP

stmtP :: Parser (Hs.Stmt ())
stmtP =   try (Hs.Generator ()
                 <$  symbol "let"
                 <*> (Hs.PVar () <$> varP)
                 <*  symbol "="
                 <*  (symbol "await" <|> symbol "do")
                 <*> exprP)
      <|> try (Hs.LetStmt () . Hs.BDecls () <$> letBindP)
      <|> Hs.Qualifier () <$> exprP

literalP :: Parser (Hs.Literal ())
literalP =   lexeme ((\c -> Hs.Char () c ("'" <> [c] <> "'"))
                         <$ single '\'' <*> L.charLiteral <* single '\'')
         <|> lexeme ((\s -> Hs.String () s ("\"" <> s <> "\""))
                         <$ char '"' <*> manyTill L.charLiteral (char '"'))
         <|> lexeme ((\i -> Hs.Int () i (show i))
                         <$> L.signed spaceConsumer (lexeme L.decimal))
         -- <|> (\i -> Hs.Frac () i (show i))
         --      <$> L.signed spaceConsumer (lexeme L.float)

-- TYPES AND CONTEXTS
-- ==================

assertionP :: Parser (Hs.Asst ())
assertionP
  =   asstNormal
        <$> parens (typeP `sepBy` comma)
        <*  colon
        <*> tyHead
  <|> asstNormal
        <$> ((: []) <$> typeP)
        <*  colon
        <*> tyHead
  where
    asstNormal :: [Hs.Type ()] -> Hs.Type () -> Hs.Asst ()
    asstNormal []     ty = Hs.TypeA () ty
    asstNormal (x:xs) ty = asstNormal xs (Hs.TyApp () ty x)

contextP :: Parser (Hs.Context ())
contextP
  = contextNormal
      <$  symbol "where"
      <*> assertionP `sepBy` comma
  where
    contextNormal []  = Hs.CxEmpty ()
    contextNormal [x] = Hs.CxSingle () x
    contextNormal xs  = Hs.CxTuple ()xs

-- TODO: create custom prelude where Type, Unit, Arrow, Equals ... are exported
tyHead :: Parser (Hs.Type ())
tyHead
  =   Hs.TyPromoted () . Hs.PromotedCon () True
                       <$ single '^' <*> qnameP litP
  <|> Hs.TyWildCard () <$ single '_' <*> optional varP
  <|> Hs.TyVar  ()     <$> varP
  <|> Hs.TyCon  ()     <$> qnameP litP

tyvarbindP :: Parser (Hs.TyVarBind ())
tyvarbindP
  =   try (Hs.KindedVar () <$> varP <* colon <*> typeP)
  <|> Hs.UnkindedVar () <$> varP

typeP :: Parser (Hs.Type ())
typeP
  =   try (tyFun <$> parens (typeP `sepBy` comma) <* arrow <*> typeP)
  <|> try (parens (Hs.TyKind () <$> typeP <* colon <*> typeP))
  <|> try ((\vars ty ctx -> Hs.TyForall () vars ctx ty)
                     <$> (Just <$> angles (tyvarbindP `sepBy` comma))
                     <*> typeP
                     <*> optional contextP)
  <|> Hs.TyStar () <$ symbol "Type"
  <|> Hs.TyList () <$ symbol "List" <*> angles typeP
  <|> tyNormal <$> tyHead
               <*> optionalOrEmpty (parens (typeP `sepBy` comma))
               <*> optionalOrEmpty (angles (typeP `sepBy` comma))

  where
    tyFun :: [Hs.Type ()] -> Hs.Type () -> Hs.Type ()
    tyFun []     t = t
    tyFun (x:xs) t = Hs.TyFun () x (tyFun xs t)
    tyNormal :: Hs.Type () -> [Hs.Type ()] -> [Hs.Type ()] -> Hs.Type ()
    tyNormal ty (a:as) rs = tyNormal (Hs.TyApp () ty a) as rs
    tyNormal ty [] (r:rs) = tyNormal (Hs.TyApp () ty r) [] rs
    tyNormal ty [] []     = ty

-- DECLARATIONS AND BINDS
-- ======================

bindsP :: Parser (Hs.Binds ())
bindsP = Hs.BDecls () <$> declsP

declsP :: Parser [Hs.Decl ()]
declsP = concat <$> some (letBindP <|> ((:[]) <$> dataDeclP))

letBindP :: Parser [Hs.Decl ()]
letBindP
  = putTogether
      <$  symbol "let"
      <*> varP
      <*> optionalOrEmpty (angles (tyvarbindP `sepBy` comma))
      <*> optionalOrEmpty (parens (argP `sepBy` comma))
      <*> optional (colon >> typeP)
      <*> optional contextP
      <*  symbol "="
      <*> exprP
  where
    putTogether :: Hs.Name () -> [Hs.TyVarBind ()]
                -> [(Hs.Name (), Maybe (Hs.Type ()))]
                -> Maybe (Hs.Type ()) -> Maybe (Hs.Context ())
                -> Hs.Exp () -> [Hs.Decl ()]
    putTogether fname tyvars args result ctx body
      = [ Hs.TypeSig () [fname] (recreateTy tyvars ctx (map snd args) result)
        , Hs.FunBind () [ Hs.Match () fname (map (Hs.PVar () . fst) args)
                                   (Hs.UnGuardedRhs () body) Nothing ] ]

    recreateTy :: [Hs.TyVarBind ()] -> Maybe (Hs.Context ())
               -> [Maybe (Hs.Type ())] -> Maybe (Hs.Type ())
               -> Hs.Type ()
    recreateTy [] Nothing args res
      = recreateTy2 args res
    recreateTy vars ctx args res
      = Hs.TyForall () (Just vars) ctx (recreateTy2 args res)

    recreateTy2 :: [Maybe (Hs.Type ())]
                -> Maybe (Hs.Type ()) -> Hs.Type ()
    recreateTy2 [] Nothing
      = Hs.TyWildCard () Nothing
    recreateTy2 [] (Just r)
      = r
    recreateTy2 (Nothing : as) r
      = Hs.TyFun () (Hs.TyWildCard () Nothing) (recreateTy2 as r)
    recreateTy2 (Just a : as) r
      = Hs.TyFun () a (recreateTy2 as r)

dataOrNewP :: Parser (Hs.DataOrNew ())
dataOrNewP
  =   Hs.DataType () <$ symbol "data"
  <|> Hs.NewType  () <$ symbol "newtype"

dataDeclP :: Parser (Hs.Decl ())
dataDeclP
  = buildDataDecl
      <$> dataOrNewP
      <*> litP
      <*> optionalOrEmpty (angles (tyvarbindP `sepBy` comma))
      <*> optional (colon *> typeP)
      <*> braces (constructorDeclP `sepBy` comma)
      <*> optionalOrEmpty (colon >> typeP `sepBy` comma)
  where
    buildDataDecl :: Hs.DataOrNew ()
                  -> Hs.Name ()
                  -> [Hs.TyVarBind ()]
                  -> Maybe (Hs.Type ())
                  -> [Hs.Type () -> Hs.GadtDecl ()]
                  -> [Hs.Type ()]
                  -> Hs.Decl ()
    buildDataDecl new tyname tyargs kind cons derivs  -- TODO deriving
      = let orig
              = foldl (\h n -> Hs.TyApp () h (Hs.TyVar () (varKindName n)))
                      (Hs.TyCon () (Hs.UnQual () tyname)) tyargs
        in Hs.GDataDecl
             () new Nothing
             (foldl (Hs.DHApp ()) (Hs.DHead () tyname) tyargs)
             kind (map ($ orig) cons)
             [Hs.Deriving () Nothing
                (map (Hs.IRule () Nothing Nothing . typeToDeriving) derivs)]

    varKindName (Hs.KindedVar _ nm _) = nm
    varKindName (Hs.UnkindedVar _ nm) = nm

    typeToDeriving :: Hs.Type () -> Hs.InstHead ()
    typeToDeriving (Hs.TyApp _ t a)
      = Hs.IHApp () (typeToDeriving t) a
    typeToDeriving (Hs.TyCon _ nm)
      = Hs.IHCon () nm

constructorDeclP :: Parser (Hs.Type () -> Hs.GadtDecl ())
constructorDeclP
  = (\nm vars flds ty ctx orig -> Hs.GadtDecl () nm vars ctx flds (ty orig))
      <$> litP
      <*> optional (angles (tyvarbindP `sepBy` comma))
      <*> optional (parens (gadtArgP `sepBy` comma))
      <*> (fromMaybe id <$> optional (const <$ colon <*> typeP))
      <*> optional contextP
  where
    gadtArgP :: Parser (Hs.FieldDecl ())
    gadtArgP
      = Hs.FieldDecl ()
          <$> some varP
          <*  colon
          <*> typeP

-- MODULES
-- =======

moduleHeadP :: Parser (Hs.ModuleHead ())
moduleHeadP
  = Hs.ModuleHead ()
      <$  symbol "module"
      <*> modNameP
      <*> pure Nothing
      <*> pure Nothing  -- TODO: export list

modulePragmaP :: Parser (Hs.ModulePragma ())
modulePragmaP
  = Hs.LanguagePragma ()
      <$  symbol "use"
      <*> litP `sepBy1` comma

moduleP :: Parser (Hs.Module ())
moduleP
  = flip (Hs.Module ())
      <$> many modulePragmaP
      <*> (Just <$> moduleHeadP)
      <*> pure [] -- TODO: import list
      <*> declsP
