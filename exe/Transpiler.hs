module Transpiler where

import Parser
import Data.List (intercalate)

-- | Main entry point for generating C code from the AST.
-- Accepts preprocessor directives and unprovable SMT expressions.
transpile :: [String] -> [Expr] -> [TopLevel] -> String
transpile directives unknowns ast = 
    let customDirectives = unlines directives
        headers = "#include <stdio.h>\n#include <stdlib.h>\n#include <stdbool.h>\n#include <assert.h>\n\n" 
                  ++ "// --- User Directives ---\n" 
                  ++ customDirectives 
                  ++ "// -----------------------"
    in headers ++ "\n\n" ++ intercalate "\n\n" (map (transpileTopLevel unknowns) ast)

transpileTopLevel :: [Expr] -> TopLevel -> String
transpileTopLevel unks tl = case tl of
    DefGlobal qual ty name mExpr -> 
        let q = if qual == Static then "static " else ""
            decl = transpileDecl ty name
            initStr = case mExpr of Just e -> " = " ++ transpileExpr e; Nothing -> ""
        in q ++ decl ++ initStr ++ ";"
        
    DefFunc f -> transpileFunc unks f
    
    DefStruct name fields -> 
        "struct " ++ name ++ " {\n" ++ 
        unlines (map (\(t, n) -> "    " ++ transpileDecl t n ++ ";") fields) ++ 
        "};"
        
    DefUnion name fields -> 
        "union " ++ name ++ " {\n" ++ 
        unlines (map (\(t, n) -> "    " ++ transpileDecl t n ++ ";") fields) ++ 
        "};"
        
    DefEnum name variants -> 
        "enum " ++ name ++ " { " ++ intercalate ", " variants ++ " };"

transpileFunc :: [Expr] -> Function -> String
transpileFunc unks (Function retTy name args mInvars mPosts body) =
    let activeInvars = filter (`elem` unks) (maybe [] id mInvars)
        activePosts  = filter (`elem` unks) (maybe [] id mPosts)
        sig = transpileDecl retTy name ++ "(" ++ intercalate ", " (map transpileArg args) ++ ")"
        preChecks = map (\e -> "    assert(" ++ transpileExpr e ++ ");") activeInvars
        bodyStr = transpileBlock unks activePosts retTy body
    in sig ++ " {\n" ++ unlines preChecks ++ bodyStr ++ "\n}"

transpileArg :: Arg -> String
transpileArg (Arg ty name) = transpileDecl ty name

transpileDecl :: Type -> String -> String
transpileDecl ty name = case ty of
    Int      -> "int " ++ name
    Double   -> "double " ++ name
    Bool     -> "bool " ++ name
    Void     -> "void " ++ name
    Char     -> "char " ++ name
    Struct n -> "struct " ++ n ++ " " ++ name
    Union n  -> "union " ++ n ++ " " ++ name
    Enum n   -> "enum " ++ n ++ " " ++ name
    Ptr t    -> transpileDecl t ("*" ++ name)
    ConstT t -> "const " ++ transpileDecl t name
    Array t mExpr -> 
        let size = case mExpr of Just e -> transpileExpr e; Nothing -> ""
        in transpileDecl t name ++ "[" ++ size ++ "]"

transpileBlock :: [Expr] -> [Expr] -> Type -> [Stmt] -> String
transpileBlock unks posts retTy stmts = unlines $ map (("    " ++) . transpileStmt unks posts retTy) stmts

transpileStmt :: [Expr] -> [Expr] -> Type -> Stmt -> String
transpileStmt unks posts retTy (Stmt _ node) = case node of
    Decl qual ty name mExpr ->
        let q = if qual == Static then "static " else ""
            decl = transpileDecl ty name
            initStr = case mExpr of Just e -> " = " ++ transpileExpr e; Nothing -> ""
        in q ++ decl ++ initStr ++ ";"
        
    Assign e1 e2 -> transpileExpr e1 ++ " = " ++ transpileExpr e2 ++ ";"
    ExprStmt e -> transpileExpr e ++ ";"
    
    If c t e -> 
        "if (" ++ transpileExpr c ++ ") {\n" ++ transpileBlock unks posts retTy t ++ "    }" ++
        case e of 
            Just els -> " else {\n" ++ transpileBlock unks posts retTy els ++ "    }"
            Nothing -> ""
            
    While c mInvars b -> 
        let loopInvars = filter (`elem` unks) (maybe [] id mInvars)
            invChecks = map (\e -> "        assert(" ++ transpileExpr e ++ ");\n") loopInvars
            bodyStr = transpileBlock unks posts retTy b
        in "while (" ++ transpileExpr c ++ ") {\n" ++ concat invChecks ++ bodyStr ++ "    }"
        
    For initS condS incS b ->
        let i = case initS of Just s -> transpileStmt unks posts retTy s; Nothing -> ";"
            c = case condS of Just e -> transpileExpr e; Nothing -> ""
            u = case incS of Just e -> transpileExpr e; Nothing -> ""
            i' = if not (null i) && last i == ';' then i else i ++ ";"
        in "for (" ++ i' ++ " " ++ c ++ "; " ++ u ++ ") {\n" ++ transpileBlock unks posts retTy b ++ "    }"
        
    Return mExpr -> 
        if null posts then
            case mExpr of Just e -> "return " ++ transpileExpr e ++ ";"; Nothing -> "return;"
        else
            let postChecks = map (\e -> "assert(" ++ transpileExpr e ++ ");\n        ") posts
            in case mExpr of
                Just e -> 
                    "{\n        " ++ transpileDecl retTy "res" ++ " = " ++ transpileExpr e ++ ";\n        " ++
                    concat postChecks ++ "return res;\n    }"
                Nothing -> 
                    "{\n        " ++ concat postChecks ++ "return;\n    }"
                    
    Break -> "break;"
    Continue -> "continue;"
    Block b -> "{\n" ++ transpileBlock unks posts retTy b ++ "    }"

transpileExpr :: Expr -> String
transpileExpr (Expr _ node) = case node of
    Var x -> x
    IntLit n -> show n
    DoubleLit d -> show d
    BoolLit True -> "true"
    BoolLit False -> "false"
    CharLit c -> show c
    StringLit s -> show s
    Index e1 e2 -> transpileExpr e1 ++ "[" ++ transpileExpr e2 ++ "]"
    ArrayUpdate _ _ _ -> "/* Error: ArrayUpdate is an internal SMT structure */"
    Call f args -> f ++ "(" ++ intercalate ", " (map transpileExpr args) ++ ")"
    Field e p -> transpileExpr e ++ "." ++ p
    Deref e -> "(*" ++ transpileExpr e ++ ")"
    AddrOf e -> "(&" ++ transpileExpr e ++ ")"
    UnOp op e -> "(" ++ op ++ transpileExpr e ++ ")"
    PreInc e -> "(++" ++ transpileExpr e ++ ")"
    PreDec e -> "(--" ++ transpileExpr e ++ ")"
    PostInc e -> "(" ++ transpileExpr e ++ "++)"
    PostDec e -> "(" ++ transpileExpr e ++ "--)"
    Ternary c t e -> "(" ++ transpileExpr c ++ " ? " ++ transpileExpr t ++ " : " ++ transpileExpr e ++ ")"
    BinOp op e1 e2 -> "(" ++ transpileExpr e1 ++ " " ++ op ++ " " ++ transpileExpr e2 ++ ")"