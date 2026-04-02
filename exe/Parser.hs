{-|
Module      : Parser
Description : Megaparsec parser for a custom language with compile-time invariants.

This module defines the Abstract Syntax Tree (AST) and the parsing rules for a 
proof-of-concept imperative language. It is specifically designed to parse 
compile-time contracts such as function pre/postconditions and loop invariants,
which are later verified by the static analyzer and Z3 SMT solver.
-}
module Parser where

import Data.Void
import Data.Maybe (isJust, isNothing)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad

-- ============================================================================
-- AST DEFINITIONS (With SourcePos Wrappers)
-- ============================================================================

-- | Supported data types in the language.
data Type
    = Int 
    | Double 
    | Bool 
    | Void 
    | Char
    | Array Type (Maybe Expr)
    | Struct String
    | Union String
    | Enum String
    | Ptr Type
    | ConstT Type
    deriving (Show, Eq)

-- | Expressions wrapped with their source position for accurate error reporting in the Analyzer.
data Expr = Expr SourcePos ExprNode deriving (Show, Eq)

-- | All possible expression nodes.
data ExprNode 
    = Var String
    | IntLit Integer
    | DoubleLit Double
    | BoolLit Bool
    | CharLit Prelude.Char
    | StringLit String
    | Index Expr Expr
    | ArrayUpdate Expr Expr Expr
    | Call String [Expr]
    | Field Expr String
    | Deref Expr
    | AddrOf Expr
    | UnOp String Expr
    | PreInc Expr
    | PreDec Expr
    | PostInc Expr
    | PostDec Expr
    | Ternary Expr Expr Expr
    | BinOp String Expr Expr
    deriving (Show, Eq)

-- | Statements wrapped with their source position.
data Stmt = Stmt SourcePos StmtNode deriving (Show, Eq)

-- | All possible statement nodes. 
-- Note the `While` loop which optionally accepts loop invariants to support the compile-time checks.
data StmtNode
    = Decl Qualifier Type String (Maybe Expr)
    | Assign Expr Expr
    | ExprStmt Expr
    | If Expr [Stmt] (Maybe [Stmt])
    | While Expr (Maybe [Expr]) [Stmt]  -- ^ While loop with optional [Invariants]
    | For (Maybe Stmt) (Maybe Expr) (Maybe Expr) [Stmt]
    | Return (Maybe Expr)
    | Break      
    | Continue   
    | Block [Stmt]
    deriving (Show, Eq)

-- | Function argument representation.
data Arg = Arg Type String deriving (Show, Eq)

-- | Variable qualifiers (e.g., static vs dynamic memory allocation).
data Qualifier = Static | Dynamic deriving (Show, Eq)

-- | Top-level declarations in a file.
data TopLevel 
    = DefGlobal Qualifier Type String (Maybe Expr) 
    | DefFunc Function 
    | DefStruct String [(Type, String)] 
    | DefUnion String [(Type, String)] 
    | DefEnum String [String]
    deriving (Show, Eq)

-- | Represents a function definition, including its compile-time contracts.
-- This directly supports the core goal of verifying constraints before runtime.
data Function = Function 
    { funcRetType   :: Type         -- ^ The return type of the function
    , funcName      :: String       -- ^ Identifier of the function
    , funcArgs      :: [Arg]        -- ^ List of parameters
    , funcInvars    :: Maybe [Expr] -- ^ Preconditions (e.g., requires n > 0)
    , funcPostconds :: Maybe [Expr] -- ^ Postconditions (e.g., ensures res != 0)
    , funcBody      :: [Stmt]       -- ^ The executable body of the function
    } deriving (Show, Eq)

-- | Megaparsec Parser type alias for parsing Strings with no custom error component.
type Parser = Parsec Void String

-- | Helper to inject dummy positions for compiler-generated AST nodes 
-- (useful during symbolic execution/AST transformations).
genExpr :: ExprNode -> Expr
genExpr = Expr (initialPos "<internal>")

-- ============================================================================
-- LEXER UTILITIES
-- ============================================================================

-- | Space consumer: handles whitespace, single-line (//), and multi-line (/* */) comments.
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Wraps a parser to automatically consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses a specific string symbol and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pInteger :: Parser Integer
pInteger = lexeme L.decimal

pDouble :: Parser Double
pDouble = lexeme L.float

pString :: Parser String
pString = char '"' *> manyTill L.charLiteral (char '"') <* sc

pChar :: Parser Prelude.Char
pChar = char '\'' *> L.charLiteral <* char '\'' <* sc

-- | List of reserved keywords to prevent them from being used as identifiers.
reserved :: [String]
reserved = 
    [ "if", "else", "while", "for", "return", "break", "continue", "struct", "union", "enum" 
    , "true", "false", "static", "const"
    , "Int", "Double", "Bool", "Void", "Char"
    ]

-- | Parses a valid identifier (alphanumeric + underscores, must start with a letter).
identifier :: Parser String
identifier = try $ do
    ident <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    if ident `elem` reserved
        then fail $ "keyword '" ++ ident ++ "' cannot be used as an identifier"
        else return ident

-- ============================================================================
-- TYPE PARSERS
-- ============================================================================

-- | Parses base types (structs, unions, enums, and primitives).
pTypeBase :: Parser Type
pTypeBase = choice
    [ try (Struct <$ symbol "struct" <*> identifier)
    , try (Union  <$ symbol "union"  <*> identifier)
    , try (Enum   <$ symbol "enum"   <*> identifier)
    , Int    <$ symbol "Int"
    , Double <$ symbol "Double"
    , Bool   <$ symbol "Bool"
    , Void   <$ symbol "Void"
    , Char   <$ symbol "Char"
    ] <?> "type"

-- | Parses a full type signature, including 'const' qualifiers and pointer ('*') modifiers.
pType :: Parser Type
pType = do
    isConstBase <- isJust <$> optional (symbol "const")
    base <- pTypeBase
    let baseTy = if isConstBase then ConstT base else base
    
    ptrModifiers <- many $ do
        void $ symbol "*"
        isConstPtr <- isJust <$> optional (symbol "const")
        return isConstPtr
        
    return $ foldl (\t isConst -> if isConst then ConstT (Ptr t) else Ptr t) baseTy ptrModifiers

pArrayDims :: Parser [Maybe Expr]
pArrayDims = many $ brackets (optional pExpr)

applyArrayDims :: Type -> [Maybe Expr] -> Type
applyArrayDims = foldl Array

pIndexModifier :: Parser (Expr -> Expr)
pIndexModifier = do
    pos <- getSourcePos
    idx <- between (symbol "[") (symbol "]") pExpr
    return (\expr -> Expr pos (Index expr idx))


-- ============================================================================
-- EXPRESSION PARSERS
-- ============================================================================

-- | Main expression parser. Handles standard expressions and the ternary (? :) operator.
pExpr :: Parser Expr
pExpr = do
    e <- makeExprParser pTerm operatorTable <?> "operator"
    mQ <- optional (symbol "?")
    case mQ of
        Nothing -> return e
        Just _ -> do
            t <- pExpr
            void $ symbol ":"
            f <- pExpr
            let (Expr pos _) = e
            return (Expr pos (Ternary e t f)) <?> "ternary expression"

-- | Parses terms, attaching postfix operators (like ++, --, array indexing, field access).
pTerm :: Parser Expr
pTerm = do
    e <- pAtom
    pPostfix e

-- | Parses atomic expressions (literals, variables, function calls, parenthesized exprs).
pAtom :: Parser Expr
pAtom = choice
    [ parens pExpr
    , try $ do
        pos <- getSourcePos
        name <- identifier
        args <- parens (pExpr `sepBy` symbol ",")
        return $ Expr pos (Call name args)
    , do
        pos <- getSourcePos
        node <- choice 
            [ DoubleLit <$> try pDouble
            , IntLit    <$> pInteger
            , BoolLit True  <$ symbol "true"
            , BoolLit False <$ symbol "false"
            , CharLit   <$> pChar
            , StringLit <$> pString
            , Var       <$> identifier 
            ]
        return (Expr pos node)
    ]

-- | Parses postfix operators recursively.
pPostfix :: Expr -> Parser Expr
pPostfix e = choice
    [ do 
        pos <- getSourcePos
        idx <- brackets pExpr
        pPostfix (Expr pos (Index e idx))
    , do 
        pos <- getSourcePos
        void $ symbol "."
        prop <- identifier
        pPostfix (Expr pos (Field e prop))
    , do 
        pos <- getSourcePos
        void $ symbol "->"
        prop <- identifier
        pPostfix (Expr pos (Field (Expr pos (Deref e)) prop))    
    , do 
        pos <- getSourcePos
        void $ symbol "++"
        pPostfix (Expr pos (PostInc e))
    , do 
        pos <- getSourcePos
        void $ symbol "--"
        pPostfix (Expr pos (PostDec e))
    , return e
    ] <?> "postfix operator"

-- | Helper for generating binary operators in the precedence table.
binary :: String -> (Expr -> Expr -> ExprNode) -> Operator Parser Expr
binary name f = InfixL $ do
    try $ symbol name
    return $ \e1@(Expr pos _) e2 -> Expr pos (f e1 e2)

-- | Helper for generating prefix operators in the precedence table.
prefix :: String -> (Expr -> ExprNode) -> Operator Parser Expr
prefix name f = Prefix $ do
    pos <- getSourcePos
    void $ symbol name
    return (\e -> Expr pos (f e)) <?> "prefix operator"

-- | Operator precedence table (highest to lowest precedence).
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ prefix "-" (UnOp "-"), prefix "!" (UnOp "!") 
      , prefix "*" Deref, prefix "&" AddrOf
      , prefix "++" PreInc, prefix "--" PreDec ]
    , [ binary "*" (BinOp "*"), binary "/" (BinOp "/"), binary "%" (BinOp "%") ]
    , [ binary "+" (BinOp "+"), binary "-" (BinOp "-") ]
    , [ binary "<=" (BinOp "<="), binary "<" (BinOp "<")
      , binary ">=" (BinOp ">="), binary ">" (BinOp ">") ]
    , [ binary "==" (BinOp "=="), binary "!=" (BinOp "!=") ]
    , [ binary "&&" (BinOp "&&") ]
    , [ binary "||" (BinOp "||") ]
    ]

-- ============================================================================
-- STATEMENT PARSERS
-- ============================================================================

-- | Parses a single statement and tags it with its source position.
pStmt :: Parser Stmt
pStmt = do
    pos <- getSourcePos
    node <- choice
        [ pBlockNode
        , pIfNode
        , pWhileNode
        , pForNode
        , pReturnNode
        , pBreakNode    
        , pContinueNode 
        , try pDeclNode
        , try pAssignNode
        , pExprStmtNode
        ]
    return $ Stmt pos node

pBlockNode :: Parser StmtNode
pBlockNode = Block <$> braces (many pStmt)

-- | Accepts either a bracketed block of statements or a single unbracketed statement.
pBlockOrStmt :: Parser [Stmt]
pBlockOrStmt = choice [ braces (many pStmt), (\s -> [s]) <$> pStmt ]

pIfNode :: Parser StmtNode
pIfNode = do
    void $ symbol "if"
    cond <- parens pExpr
    thn <- pBlockOrStmt
    els <- optional (symbol "else" *> pBlockOrStmt)
    return $ If cond thn els

-- | Parses a while loop. Notice the optional invariants section `(expr, expr)` 
-- injected between the condition and the body for compile-time verification.
pWhileNode :: Parser StmtNode
pWhileNode = do
    void $ symbol "while"
    cond <- parens pExpr
    invars <- optional (parens (pExpr `sepBy` symbol ","))
    body <- pBlockOrStmt
    return $ While cond invars body

pForNode :: Parser StmtNode
pForNode = do
    void $ symbol "for"
    void $ symbol "("
    initStmt <- optional pStmt
    when (isNothing initStmt) (void $ symbol ";")
    cond <- optional pExpr
    void $ symbol ";"
    inc <- optional pExpr
    void $ symbol ")"
    body <- pBlockOrStmt
    return $ For initStmt cond inc body

pReturnNode :: Parser StmtNode
pReturnNode = do
    void $ symbol "return"
    mExpr <- optional pExpr
    void $ symbol ";"
    return $ Return mExpr

pBreakNode :: Parser StmtNode
pBreakNode = Break <$ symbol "break" <* symbol ";"

pContinueNode :: Parser StmtNode
pContinueNode = Continue <$ symbol "continue" <* symbol ";"

-- | Parses variable declarations with an optional initialization expression.
pDeclNode :: Parser StmtNode
pDeclNode = do
    qual <- option Dynamic (Static <$ symbol "static")
    ty <- pType
    name <- identifier
    dims <- pArrayDims
    let finalTy = applyArrayDims ty dims
    mExpr <- optional (symbol "=" *> pExpr)
    void $ symbol ";"
    return $ Decl qual finalTy name mExpr

pAssignNode :: Parser StmtNode
pAssignNode = do
    lhs <- pExpr
    void $ symbol "="
    rhs <- pExpr
    void $ symbol ";"
    return $ Assign lhs rhs

pExprStmtNode :: Parser StmtNode
pExprStmtNode = do
    e <- pExpr
    void $ symbol ";"
    return $ ExprStmt e

-- ============================================================================
-- TOP LEVEL & FUNCTION PARSERS
-- ============================================================================

pArg :: Parser Arg
pArg = do
    baseTy <- pType
    name <- identifier
    dims <- pArrayDims
    return $ Arg (applyArrayDims baseTy dims) name

-- | Parses a complete function. 
-- Supports optional precondition (`funcInvars`) and postcondition (`funcPostconds`) 
-- blocks directly after the arguments, fueling the symbolic execution engine.
pFunction :: Parser Function
pFunction = do
    retTy <- pType
    name <- identifier
    args <- parens (pArg `sepBy` symbol ",")
    mInvars <- optional (parens (pExpr `sepBy` symbol ","))
    mPostconds <- optional (parens (pExpr `sepBy` symbol ","))
    body <- braces (many pStmt)
    return (Function retTy name args mInvars mPostconds body)

pGlobalDef :: Parser TopLevel
pGlobalDef = do
    qual <- option Dynamic (Static <$ symbol "static")
    ty <- pType
    name <- identifier
    dims <- pArrayDims
    let finalTy = applyArrayDims ty dims
    mExpr <- optional (symbol "=" *> pExpr)
    void $ symbol ";"
    return $ DefGlobal qual finalTy name mExpr

pFieldDef :: Parser (Type, String)
pFieldDef = do
    baseTy <- pType
    name <- identifier
    dims <- pArrayDims
    void $ symbol ";"
    return (applyArrayDims baseTy dims, name)

pStructDef :: Parser TopLevel
pStructDef = symbol "struct" *> (DefStruct <$> identifier <*> braces (many pFieldDef)) <* symbol ";"

pUnionDef :: Parser TopLevel
pUnionDef = symbol "union" *> (DefUnion <$> identifier <*> braces (many pFieldDef)) <* symbol ";"

pEnumDef :: Parser TopLevel
pEnumDef = symbol "enum" *> (DefEnum <$> identifier <*> braces (identifier `sepBy` symbol ",")) <* symbol ";"

-- | Parses any top-level declaration in the file.
pTopLevel :: Parser TopLevel
pTopLevel = choice
    [ try (DefFunc <$> pFunction)
    , try pStructDef
    , try pUnionDef
    , try pEnumDef 
    , try pGlobalDef
    ]

-- | Root parser for an entire source file.
pFile :: Parser [TopLevel]
pFile = sc *> many pTopLevel <* eof