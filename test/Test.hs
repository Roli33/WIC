module Main where

import Test.Hspec
import Text.Megaparsec
import Parser
import Analyzer


-- ============================================================================
-- AST POSITION STRIPPING UTILITIES (For clean structural testing)
-- ============================================================================

clearPos :: SourcePos
clearPos = initialPos "test-input"

cP_E :: Expr -> Expr
cP_E (Expr _ node) = Expr clearPos $ case node of
    Index e1 e2 -> Index (cP_E e1) (cP_E e2)
    Call n args -> Call n (map cP_E args)
    Field e p -> Field (cP_E e) p
    Deref e -> Deref (cP_E e)
    AddrOf e -> AddrOf (cP_E e)
    UnOp op e -> UnOp op (cP_E e)
    PreInc e -> PreInc (cP_E e)
    PostInc e -> PostInc (cP_E e)
    PreDec e -> PreDec (cP_E e)
    PostDec e -> PostDec (cP_E e)
    Ternary c t e -> Ternary (cP_E c) (cP_E t) (cP_E e)
    BinOp op e1 e2 -> BinOp op (cP_E e1) (cP_E e2)
    _ -> node

-- FIXED: We must traverse into Array/Ptr/Const boundaries 
-- to recursively clear expressions embedded as sizes.
cP_T :: Type -> Type
cP_T ty = case ty of
    Array t mE -> Array (cP_T t) (fmap cP_E mE)
    Ptr t -> Ptr (cP_T t)
    ConstT t -> ConstT (cP_T t)
    _ -> ty

cP_S :: Stmt -> Stmt
cP_S (Stmt _ node) = Stmt clearPos $ case node of
    Decl q t n mE -> Decl q (cP_T t) n (fmap cP_E mE)
    Assign e1 e2 -> Assign (cP_E e1) (cP_E e2)
    ExprStmt e -> ExprStmt (cP_E e)
    If c thn els -> If (cP_E c) (map cP_S thn) (fmap (map cP_S) els)
    While c invs b -> While (cP_E c) (fmap (map cP_E) invs) (map cP_S b)
    For initS c inc b -> For (fmap cP_S initS) (fmap cP_E c) (fmap cP_E inc) (map cP_S b)
    Return mE -> Return (fmap cP_E mE)
    Block stmts -> Block (map cP_S stmts)
    Break -> Break
    Continue -> Continue

cP_A :: Parser.Arg -> Parser.Arg
cP_A (Arg t n) = Arg (cP_T t) n

cP_F :: Function -> Function
cP_F (Function rt n args invs posts body) = 
    Function (cP_T rt) n (map cP_A args) (fmap (map cP_E) invs) (fmap (map cP_E) posts) (map cP_S body)

cP_TL :: TopLevel -> TopLevel
cP_TL tl = case tl of
    DefStruct n fields -> DefStruct n (map (\(t, f) -> (cP_T t, f)) fields)
    DefUnion n fields -> DefUnion n (map (\(t, f) -> (cP_T t, f)) fields)
    DefEnum n variants -> DefEnum n variants
    DefFunc f -> DefFunc (cP_F f)
    DefGlobal q t n mE -> DefGlobal q (cP_T t) n (fmap cP_E mE)

-- AST Construction Helpers (Creates nodes with zeroed positions)
eInt :: Integer -> Expr
eInt n = Expr clearPos (IntLit n)

eDouble :: Double -> Expr
eDouble d = Expr clearPos (DoubleLit d)

eBool :: Bool -> Expr
eBool b = Expr clearPos (BoolLit b)

eChar :: Char -> Expr
eChar c = Expr clearPos (CharLit c)

eString :: String -> Expr
eString s = Expr clearPos (StringLit s)

eVar :: String -> Expr
eVar x = Expr clearPos (Var x)

eBin :: String -> Expr -> Expr -> Expr
eBin op e1 e2 = Expr clearPos (BinOp op e1 e2)

eUn :: String -> Expr -> Expr
eUn op e = Expr clearPos (UnOp op e)

eCall :: String -> [Expr] -> Expr
eCall f args = Expr clearPos (Call f args)

eIndex :: Expr -> Expr -> Expr
eIndex e i = Expr clearPos (Index e i)

eField :: Expr -> String -> Expr
eField e f = Expr clearPos (Field e f)

eAddr :: Expr -> Expr
eAddr e = Expr clearPos (AddrOf e)

eDeref :: Expr -> Expr
eDeref e = Expr clearPos (Deref e)

ePreInc, ePostInc, ePreDec, ePostDec :: Expr -> Expr
ePreInc e = Expr clearPos (PreInc e)
ePostInc e = Expr clearPos (PostInc e)
ePreDec e = Expr clearPos (PreDec e)
ePostDec e = Expr clearPos (PostDec e)

eTernary :: Expr -> Expr -> Expr -> Expr
eTernary c t e = Expr clearPos (Ternary c t e)

sDecl :: Qualifier -> Type -> String -> Maybe Expr -> Stmt
sDecl q t n e = Stmt clearPos (Decl q t n e)

sAssign :: Expr -> Expr -> Stmt
sAssign e1 e2 = Stmt clearPos (Assign e1 e2)

sExpr :: Expr -> Stmt
sExpr e = Stmt clearPos (ExprStmt e)

sIf :: Expr -> [Stmt] -> Maybe [Stmt] -> Stmt
sIf c t e = Stmt clearPos (If c t e)

sWhile :: Expr -> Maybe [Expr] -> [Stmt] -> Stmt
sWhile c i b = Stmt clearPos (While c i b)

sFor :: Maybe Stmt -> Maybe Expr -> Maybe Expr -> [Stmt] -> Stmt
sFor i c u b = Stmt clearPos (For i c u b)

sReturn :: Maybe Expr -> Stmt
sReturn e = Stmt clearPos (Return e)

sBreak, sContinue :: Stmt
sBreak = Stmt clearPos Break
sContinue = Stmt clearPos Continue

sBlock :: [Stmt] -> Stmt
sBlock b = Stmt clearPos (Block b)

-- ============================================================================
-- PARSER HELPERS
-- ============================================================================

parseOk :: Parser a -> String -> a
parseOk p input = case parse (sc *> p <* eof) "test-input" input of
    Left err  -> error $ errorBundlePretty err
    Right res -> res

parseOk' :: String -> [TopLevel]
parseOk' input = case parse (sc *> pFile <* eof) "test-input" input of
    Left err  -> error $ errorBundlePretty err
    Right res -> res

parseFails :: Show a => Parser a -> String -> Expectation
parseFails p input = case parse (sc *> p <* eof) "test-input" input of
    Left err -> do
        putStrLn "\n--- Expected Parse Error ---"
        putStrLn (errorBundlePretty err)
        putStrLn "----------------------------"
    Right res -> 
        expectationFailure $ "Expected the parse to fail, but it succeeded and built this AST: " ++ show res

-- ============================================================================
-- TEST SUITE
-- ============================================================================

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Expression Parser (pExpr)" $ do
        it "respects standard operator precedence" $ do
            cP_E (parseOk pExpr "1 + 2 * 3") `shouldBe` 
                eBin "+" (eInt 1) (eBin "*" (eInt 2) (eInt 3))
                
        it "handles postfix and prefix operators together" $ do
            cP_E (parseOk pExpr "*ptr++") `shouldBe` 
                eDeref (ePostInc (eVar "ptr"))

        it "parses character and string literals" $ do
            cP_E (parseOk pExpr "'a'") `shouldBe` eChar 'a'
            cP_E (parseOk pExpr "'\\n'") `shouldBe` eChar '\n'
            cP_E (parseOk pExpr "\"hello world\"") `shouldBe` eString "hello world"

        it "parses array indexing (single and multi-dimensional)" $ do
            cP_E (parseOk pExpr "arr[5]") `shouldBe` 
                eIndex (eVar "arr") (eInt 5)
            cP_E (parseOk pExpr "matrix[i][j + 1]") `shouldBe` 
                eIndex (eIndex (eVar "matrix") (eVar "i")) (eBin "+" (eVar "j") (eInt 1))
                
        it "parses the ternary operator" $ do
            cP_E (parseOk pExpr "x > 0 ? 1 : 0") `shouldBe` 
                eTernary (eBin ">" (eVar "x") (eInt 0)) (eInt 1) (eInt 0)

    describe "Statement Parser (pStmt)" $ do
        it "parses break and continue statements" $ do
            cP_S (parseOk pStmt "break;") `shouldBe` sBreak
            cP_S (parseOk pStmt "continue;") `shouldBe` sContinue

        it "parses standalone block scopes" $ do
            cP_S (parseOk pStmt "{ Int x = 5; break; }") `shouldBe` 
                sBlock [ sDecl Dynamic Int "x" (Just (eInt 5))
                       , sBreak ]

        it "parses various const pointer configurations" $ do
            cP_S (parseOk pStmt "const Int x = 1;") `shouldBe`
                sDecl Dynamic (ConstT Int) "x" (Just (eInt 1))
                
            cP_S (parseOk pStmt "const Int * p;") `shouldBe`
                sDecl Dynamic (Ptr (ConstT Int)) "p" Nothing
                
            cP_S (parseOk pStmt "const Int * const p;") `shouldBe`
                sDecl Dynamic (ConstT (Ptr (ConstT Int))) "p" Nothing

        it "parses standard initialized declarations" $ do
            cP_S (parseOk pStmt "Int x = 5;") `shouldBe` 
                sDecl Dynamic Int "x" (Just (eInt 5))
                
        it "parses static pointer declarations" $ do
            cP_S (parseOk pStmt "static Int *ptr = &x;") `shouldBe` 
                sDecl Static (Ptr Int) "ptr" (Just (eAddr (eVar "x")))

        it "parses array declarations with and without size" $ do
            cP_S (parseOk pStmt "Char buffer[256];") `shouldBe` 
                sDecl Dynamic (Array Char (Just (eInt 256))) "buffer" Nothing
            cP_S (parseOk pStmt "Char msg[] = \"hi\";") `shouldBe` 
                sDecl Dynamic (Array Char Nothing) "msg" (Just (eString "hi"))
                
        it "parses assigning to an array index" $ do
            cP_S (parseOk pStmt "arr[0] = 100;") `shouldBe`
                sAssign (eIndex (eVar "arr") (eInt 0)) (eInt 100)
                
        it "parses for-loops with expression updates" $ do
            cP_S (parseOk pStmt "for(Int i = 0; i < 10; i++) { return i; }") `shouldBe` 
                sFor (Just (sDecl Dynamic Int "i" (Just (eInt 0)))) 
                     (Just (eBin "<" (eVar "i") (eInt 10))) 
                     (Just (ePostInc (eVar "i"))) 
                     [sReturn (Just (eVar "i"))]

    describe "Top-Level Parser (pFile)" $ do
        it "parses struct definitions containing array fields" $ do
            let input = "struct Network { Char ip[16]; Int ports[]; };"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [DefStruct "Network" 
                    [ (Array Char (Just (eInt 16)), "ip")
                    , (Array Int Nothing, "ports") 
                    ]]
                
        it "parses functions accepting arrays" $ do
            let input = "Void process(Char data[], Int sizes[10]) { return; }"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [ DefFunc (Function 
                    { funcRetType = Void
                    , funcName = "process"
                    , funcArgs = [ Arg (Array Char Nothing) "data"
                                 , Arg (Array Int (Just (eInt 10))) "sizes" ]
                    , funcInvars = Nothing
                    , funcPostconds = Nothing
                    , funcBody = [sReturn Nothing]
                    })
                ]

        it "parses block comments hiding code" $ do
            let input = "/* \n Int hidden = 5; \n */\n Int visible = 10;"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [DefGlobal Dynamic Int "visible" (Just (eInt 10))]

        it "parses global variable declarations" $ do
            let input = "static const Int MAX_SIZE = 100;"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [DefGlobal Static (ConstT Int) "MAX_SIZE" (Just (eInt 100))]
                
        it "parses uninitialized global arrays" $ do
            let input = "Char globalBuffer[1024];"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [DefGlobal Dynamic (Array Char (Just (eInt 1024))) "globalBuffer" Nothing]

        it "parses functions with both preconditions and postconditions" $ do
            let input = "Int process(Int n)(n > 0)(res != 0) { return res; }"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [ DefFunc (Function 
                    { funcRetType = Int
                    , funcName = "process"
                    , funcArgs = [Arg Int "n"]
                    , funcInvars = Just [eBin ">" (eVar "n") (eInt 0)]
                    , funcPostconds = Just [eBin "!=" (eVar "res") (eInt 0)]
                    , funcBody = [sReturn (Just (eVar "res"))]
                    })
                ]

        it "parses functions with ONLY postconditions (empty precondition block)" $ do
            let input = "Int init()()(ready == true) { return 1; }"
            map cP_TL (parseOk pFile input) `shouldBe` 
                [ DefFunc (Function 
                    { funcRetType = Int
                    , funcName = "init"
                    , funcArgs = []
                    , funcInvars = Just [] 
                    , funcPostconds = Just [eBin "==" (eVar "ready") (eBool True)]
                    , funcBody = [sReturn (Just (eInt 1))]
                    })
                ]

    describe "Negative Tests (Expected Failures)" $ do
        it "rejects variable names starting with numbers" $ do
            parseFails pStmt "Int 5x = 10;"
            
        it "rejects declarations missing a semicolon" $ do
            parseFails pStmt "Int x = 5"
            
        it "rejects malformed if-statements (missing closing parenthesis)" $ do
            parseFails pStmt "if (x > 0 { return 1; }"
            
        it "rejects invalid operators in expressions" $ do
            parseFails pExpr "x @ y"
            
        it "rejects unknown types in declarations" $ do
            parseFails pStmt "String myName = 5;"
            
        it "rejects incomplete compile-time invariants" $ do
            let input = "Int main(Int n)(n > ) { return 0; }"
            parseFails pFile input

        it "rejects unclosed string literals" $ do
            parseFails pExpr "\"hello"
            
        it "rejects malformed array declarations (missing closing bracket)" $ do
            parseFails pStmt "Int arr[10 = 0;"

        it "rejects using types as variable names" $ do
            parseFails pStmt "Int Int = 5;"
            
        it "rejects using control flow keywords as variable names" $ do
            parseFails pStmt "Double while = 3.14;"
            
        it "rejects struct definitions using keywords as names" $ do
            parseFails pFile "struct enum { Int x; };"


    -- ========================================================================
    -- ANALYZER TESTS (Using String inclusions due to SourcePos Tracking)
    -- ========================================================================

    describe "Compile-Time Invariant Analyzer" $ do
        
        it "catches invariant violations based on hardcoded values" $ do
            let input = unlines
                    [ "Int divide(Int a, Int b)(b > 0) { return a / b; }"
                    , "Int main() {"
                    , "    Int x = 10;"
                    , "    Int y = 0;"
                    , "    divide(x, y);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` (["test-input:5:5 - Precondition Failed in: divide: y > 0"],[])

        it "satisfies preconditions using postconditions of other functions" $ do
            let input = unlines
                    [ "Int getSafeNumber()()(res == 5) { return 5; }"
                    , "Int process(Int n)(n > 0) { return n; }"
                    , "Int main() {"
                    , "    Int x = getSafeNumber();" 
                    , "    process(x);" 
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "Compile-Time Symbolic Analyzer" $ do
        
        it "proves complex constraints (x * y == 10) by translating scopes" $ do
            let input = unlines
                    [ "Void generate(Int a, Int b)()(a * b == 10) { return; }"
                    , "Void consume(Int x, Int y)(x * y == 10) { return; }"
                    , "Int main() {"
                    , "    Int p = 0; Int q = 0;"
                    , "    generate(p, q);"     
                    , "    consume(p, q);"      
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "proves inequalities using heuristics (x > 10 implies x > 5)" $ do
            let input = unlines
                    [ "Int getLargeNumber()()(res > 10) { return 15; }"
                    , "Void process(Int val)(val > 5) { return; }"
                    , "Int main() {"
                    , "    Int num = getLargeNumber();"
                    , "    process(num);"               
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "flags unprovable constraints" $ do
            let input = unlines
                    [ "Int getSmallNumber()()(res < 3) { return 2; }"
                    , "Void process(Int val)(val > 5) { return; }"
                    , "Int main() {"
                    , "    Int num = getSmallNumber();" 
                    , "    process(num);"               
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:5:5 - Precondition Failed in: process: num > 5"]
            unknowns `shouldBe` []

        it "resolves equations using Equality Substitution" $ do
            let input = unlines
                    [ "Void assignSync(Int a, Int b)()(a == b) { return; }"
                    , "Void requiresGreater(Int target)(target > 100) { return; }"
                    , "Int main() {"
                    , "    Int x = 150;"
                    , "    Int y = 0;"
                    , "    assignSync(x, y);"    
                    , "    requiresGreater(y);"  
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "spots implicit postconditions via sandbox execution (resolving local variables)" $ do
            let input = unlines
                    [ "Int getHiddenSafe() {"
                    , "    Int x = 10;"
                    , "    Int y = 5;"
                    , "    return x + y;"
                    , "}"
                    , "Void requireSafe(Int val)(val == 15) { return; }"
                    , "Int main() {"
                    , "    Int target = getHiddenSafe();"
                    , "    requireSafe(target);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])
            
        it "spots implicit postconditions via symbolic AST inference" $ do
            let input = unlines
                    [ "Int add(Int a, Int b) { return a + b; }" 
                    , "Void process(Int val, Int x, Int y)(val == x + y) { return; }"
                    , "Int main() {"
                    , "    Int p = 0; Int q = 0;"
                    , "    Int result = add(p, q);"
                    , "    process(result, p, q);" 
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "resolves Weakest Postconditions across branching if/else paths" $ do
            let input = unlines
                    [ "Int absolute(Int n) {"
                    , "    if (n < 0) { return -n; }"
                    , "    else       { return n; }"
                    , "}"
                    , "Void demandPositive(Int val)(val >= 0) { return; }"
                    , "Int main() {"
                    , "    Int x = test();"
                    , "    Int absX = absolute(x);" 
                    , "    demandPositive(absX);" 
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "preserves value if nothing changes" $ do
            let input = unlines
                    [ "Void clobber(Int *p) { return; }"
                    , "Void require(Int val)(val == 100) { return; }"
                    , "Int main() {"
                    , "    Int x = 100;"
                    , "    clobber(&x);"
                    , "    require(x);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "deduces updated postconditions from pointer mutations" $ do
            let input = unlines
                    [ "Void setZero(Int *p) { *p = 0; return; }"
                    , "Void requireZero(Int val)(val == 0) { return; }"
                    , "Int main() {"
                    , "    Int x = 100;"
                    , "    setZero(&x);" 
                    , "    requireZero(x);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "1. Concrete Evaluation & Basic Preconditions" $ do
        it "passes when concrete variables satisfy preconditions" $ do
            let input = unlines
                    [ "Void requirePositive(Int val)(val > 0) { return; }"
                    , "Int main() {"
                    , "    Int x = 10;"
                    , "    requirePositive(x);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "fails and logs violation when concrete variables fail preconditions" $ do
            let input = unlines
                    [ "Void requirePositive(Int val)(val > 0) { return; }"
                    , "Int main() {"
                    , "    Int x = -5;"
                    , "    requirePositive(x);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:4:5 - Precondition Failed in: requirePositive: x > 0"]
            unknowns `shouldBe` []

    describe "4. Weakest Postconditions & Control Flow" $ do
        it "resolves simple implicit postconditions from pure functions" $ do
            let input = unlines
                    [ "Int add(Int a, Int b) { return a + b; }"
                    , "Void require(Int val, Int x, Int y)(val == x + y) { return; }"
                    , "Int main() {"
                    , "    Int p = 0; Int q = 0;"
                    , "    Int sum = add(p, q);"
                    , "    require(sum, p, q);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "5. Pointers & Memory Side-Effects" $ do
        it "invalidates concrete and symbolic facts when variables are passed by reference" $ do
            let input = unlines
                    [ "Void require(Int val)(val == 10) { return; }"
                    , "Int main() {"
                    , "    Int x = 10;"
                    , "    clobber(&x);" 
                    , "    require(x);" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["x == 10"]

        it "fails if a pointer branch leaves the requirement unprovable" $ do
            let input = unlines
                    [ "Void toggle(Int *state, Int flag) {"
                    , "    if (flag == 1) { *state = 100; return; }"
                    , "    else           { *state = -100; return; }"
                    , "}"
                    , "Void require(Int val)(val == 100) { return; }"
                    , "Int main() {"
                    , "    Int s = 0;"
                    , "    Int f = 0;" 
                    , "    toggle(&s, f);" 
                    , "    require(s);" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:10:5 - Precondition Failed in: require: s == 100"]
            unknowns `shouldBe` []

    describe "7. Advanced Data Structures (Arrays, Structs, Unions, Enums)" $ do
        it "flags violations when struct fields fail postconditions" $ do
            let input = unlines
                    [ "struct Point { Int x; Int y; };"
                    , "struct Point getPoint()()(res.x == -5) { struct Point p; return p; }"
                    , "Void requirePositiveX(struct Point p)(p.x > 0) { return; }"
                    , "Int main() {"
                    , "    struct Point pt = getPoint();"
                    , "    requirePositiveX(pt);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:6:5 - Precondition Failed in: requirePositiveX: pt.x > 0"]
            map prettyTop unknowns `shouldBe` ["res.x == -5"]

        it "flags violations on mismatched array logic" $ do
            let input = unlines
                    [ "Int getFirstElement(Int arr[])()(res == arr[0]) { return arr[0]; }"
                    , "Void requireMatch(Int a, Int b)(a == b) { return; }"
                    , "Int main() {"
                    , "    Int buffer[10];"
                    , "    Int first = getFirstElement(buffer);"
                    , "    requireMatch(first, buffer[1]);" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["first == buffer[1]"]

        it "parses enums globally and tracks state" $ do
            let input = unlines
                    [ "enum State { IDLE, RUNNING, ERROR };"
                    , "Int getState()()(res == 1) { return 1; }"
                    , "Void requireRunning(Int s)(s == 1) { return; }"
                    , "Int main() {"
                    , "    Int s = getState();"
                    , "    requireRunning(s);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "8. Formal Loop Induction" $ do
        it "catches a failing Inductive Step (invariant doesn't hold after body)" $ do
            let input = unlines
                    [ "Void test() {"
                    , "    Int i = 0;"
                    , "    while (i < 10) (i == 0) {"
                    , "        i = i - 1;"
                    , "    }"
                    , "    return;"
                    , "}"
                    , "Int main() { test(); return 0; }"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:3:5 - Loop Inductive Step Failed: i == 0"]
            unknowns `shouldBe` []

    describe "Type-Aware SMT (Reals and Booleans)" $ do
        it "proves exact floating-point / real arithmetic without integer truncation" $ do
            let input = unlines
                    [ "Void requireDouble(Double d)(d > 2.5) { return; }"
                    , "Int main() {"
                    , "    Double a = 1.5;"
                    , "    Double b = 1.1;"
                    , "    requireDouble(a + b);" 
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "Safety Extraction (Div-by-Zero & Array Bounds)" $ do
        it "catches potential division by zero at compile time" $ do
            let input = unlines
                    [ "Int divide(Int a, Int b) { return a / b; }"
                    , "Int main() {"
                    , "    Int x = 10; Int y = 0;"
                    , "    divide(x, y);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["b != 0"]

        it "catches array out-of-bounds writes using Z3 bounds checking" $ do
            let input = unlines
                    [ "Int main() {"
                    , "    Int arr[5];"
                    , "    arr[10] = 99;" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:3:8 - Array Index Out of Bounds: 10 < 5"]
            unknowns `shouldBe` []

        it "proves an array access is safe based on conditional logic" $ do
            let input = unlines
                    [ "Int safeAccess(Int arr[5], Int idx) {"
                    , "    if (idx >= 0 && idx < 5) {"
                    , "        return arr[idx];" 
                    , "    }"
                    , "    return -1;"
                    , "}"
                    , "Int main() { return 0; }"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "Uninterpreted Functions (Determinism & Opaqueness)" $ do
        it "functions aren't necessarily pure" $ do
            let input = unlines
                    [ "Void requireEq(Int a, Int b)(a == b) { return; }"
                    , "Int main() {"
                    , "    Int val1 = readSensor(42);"
                    , "    Int val2 = readSensor(42);"
                    , "    requireEq(val1, val2);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["val1 == val2"]

        it "functions aren't necessarily pure v2" $ do
            let input = unlines
                    [ "Void requireEq(Int a, Int b)(a != b) { return; }"
                    , "Int main() {"
                    , "    Int val1 = readSensor(42);"
                    , "    Int val2 = readSensor(42);"
                    , "    requireEq(val1, val2);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["val1 != val2"]

        it "fails if opaque functions are called with different inputs" $ do
            let input = unlines
                    [ "Int readSensor(Int port) {}" 
                    , "Void requireEq(Int a, Int b)(a == b) { return; }"
                    , "Int main() {"
                    , "    Int val1 = readSensor(1);"
                    , "    Int val2 = readSensor(2);"
                    , "    requireEq(val1, val2);" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` []
            map prettyTop unknowns `shouldBe` ["val1 == val2"]

    describe "Z3 Theory of Arrays (Pointer/Memory Aliasing)" $ do
        it "proves memory writes don't overlap if indices are distinct" $ do
            let input = unlines
                    [ "Void requireMatch(Int a)(a == 10) { return; }"
                    , "Int main() {"
                    , "    Int memory[100];"
                    , "    Int p = 5;"
                    , "    Int q = 10;"
                    , "    memory[p] = 10;"
                    , "    memory[q] = 20;" 
                    , "    requireMatch(memory[p]);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "Bounded Model Checking (BMC for Unannotated Loops)" $ do
        it "automatically unrolls and proves loops without requiring user invariants" $ do
            let input = unlines
                    [ "Void requireMatch(Int x)(x == 3) { return; }"
                    , "Int main() {"
                    , "    Int i = 0;"
                    , "    while (i < 3) {" 
                    , "        i = i + 1;"
                    , "    }"
                    , "    requireMatch(i);" 
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "catches unrolled loop bounds failures" $ do
            let input = unlines
                    [ "Void requireMatch(Int x)(x == 10) { return; }"
                    , "Int main() {"
                    , "    Int i = 0;"
                    , "    while (i < 3) { i = i + 1; }"
                    , "    requireMatch(i);" 
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            errors `shouldBe` ["test-input:5:5 - Precondition Failed in: requireMatch: i == 10"]
            unknowns `shouldBe` []
        
        it "projects inequality bounds from path conditions onto the return value" $ do
            let input = unlines
                    [ "Int getBounded(Int limit) {"
                    , "    Int sensor = readSensor();" -- Opaque call, value completely unknown!
                    , "    if (sensor < limit) {"
                    , "        return sensor;" -- Derives res == sensor AND projects: res < limit
                    , "    } else {"
                    , "        return limit;"  -- Derives res == limit AND !(sensor < limit) -> sensor >= limit
                    , "    }"
                    , "}" --  (res == sensor AND projects: res < limit) || (res == limit AND !(sensor < limit) -> sensor >= limit) AND limit == l
                    , "Void require(Int val, Int max)(val <= max) { return; }"
                    , "Int main() {"
                    , "    Int l = 100;"
                    , "    Int val = getBounded(l);"
                    , "    require(val, l);" -- Passes because both Path 1 (<) and Path 2 (==) satisfy <=
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "projects inequality bounds from path conditions onto the return value" $ do
            let input = unlines
                    [ "Int getBounded(Int limit) {"
                    , "    Int sensor = readSensor();"
                    , "    Int a = 1;"
                    , "    if (sensor < limit) {"
                    , "        a = sensor;"
                    , "    } else {"
                    , "        sensor = limit;"
                    , "    }"
                    , "    a = limit;"
                    , "    return a * sensor;"
                    , "}"
                    , "Void require(Int val, Int max)(val <= max * max) { return; }"
                    , "Int main() {"
                    , "    Int l = 100;"
                    , "    Int val = getBounded(l);"
                    , "    require(val, l);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "Sandbox Memory Management & Local Aliasing" $ do
        
        it "ensures local variables do not alias with parameters in memory" $ do
            let input = unlines
                    [ "Int calculateDifference(Int maxVal, Int minVal) {"
                    , "    Int diff = maxVal - minVal;" -- If diff aliases maxVal, diff becomes maxVal - minVal, but maxVal is also overwritten!
                    , "    return diff;"
                    , "}"
                    , "Void require(Int val)(val == 15) { return; }"
                    , "Int main() {"
                    , "    Int m1 = 25;"
                    , "    Int m2 = 10;"
                    , "    Int res = calculateDifference(m1, m2);"
                    , "    require(res);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "evaluates independent path conditions without memory collisions between multiple params and locals" $ do
            let input = unlines
                    [ "Int constrain(Int val, Int minVal, Int maxVal) {"
                    , "    Int localMin = minVal;" -- If localMin aliases val, val becomes minVal, breaking the > localMax check.
                    , "    Int localMax = maxVal;"
                    , "    if (val < localMin) { return localMin; }"
                    , "    if (val > localMax) { return localMax; }"
                    , "    return val;"
                    , "}"
                    , "Void requireRange(Int v, Int low, Int high)(v >= low && v <= high) { return; }"
                    , "Int main() {"
                    , "    Int unk = readSensor();"
                    , "    Int bottom = 0;"
                    , "    Int top = 255;"
                    , "    Int res = constrain(unk, bottom, top);"
                    , "    requireRange(res, bottom, top);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

        it "preserves parameter values when local variables are declared inside branches" $ do
            let input = unlines
                    [ "Int max_val(Int a, Int b) {"
                    , "    Int threshold = 100;"
                    , "    if (a > b) {"
                    , "        Int tempA = a;"
                    , "        return tempA;"
                    , "    } else {"
                    , "        Int tempB = b;"
                    , "        return tempB;"
                    , "    }"
                    , "}"
                    , "Void require(Int val)(val == 50) { return; }"
                    , "Int main() {"
                    , "    Int x = 10;"
                    , "    Int y = 50;"
                    , "    Int res = max_val(x, y);"
                    , "    require(res);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])
            
        it "safely handles shadowing of variables in inner blocks without destroying parameter bounds" $ do
            let input = unlines
                    [ "Int shadowTest(Int target) {"
                    , "    Int val = readSensor();"
                    , "    if (val > target) {"
                    , "        Int target = 0;" -- Inner block shadowing
                    , "        return val + target;"
                    , "    }"
                    , "    return val;"
                    , "}"
                    , "Void requireSafe(Int v, Int t)(v > t || v <= t) { return; }"
                    , "Int main() {"
                    , "    Int limit = 10;"
                    , "    Int result = shadowTest(limit);"
                    , "    requireSafe(result, limit);"
                    , "    return 0;"
                    , "}"
                    ]
            errors <- runAnalyzer (parseOk' input)
            errors `shouldBe` ([],[])

    describe "10. Unresolved Constraints & Runtime Asserts (Unknowns)" $ do
        
        it "captures unprovable properties as unknowns rather than fatal compile errors" $ do
            let input = unlines
                    [ "Void requiresMagic(Int x, Int y, Int z)(x*x*x + y*y*y != z*z*z) { return; }"
                    , "Int main() {"
                    , "    Int a = readSensor();"
                    , "    Int b = readSensor();"
                    , "    Int c = readSensor();"
                    , "    requiresMagic(a, b, c);" 
                    , "    return 0;"
                    , "}"
                    ]
            
            -- Non-linear integer arithmetic (like Fermat's Last Theorem above) 
            -- is notoriously undecidable for SMT solvers, forcing Z3 to return Undef (Unknown).
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            
            errors `shouldBe` []
            
            -- Extract the mathematical expression that was deferred to runtime
            map prettyTop unknowns `shouldBe` ["(((a * a) * a) + ((b * b) * b)) != ((c * c) * c)"]


        it "aborts compilation for PROVABLY false constraints, keeping unknowns empty" $ do
            let input = unlines
                    [ "Void requirePositive(Int val)(val > 0) { return; }"
                    , "Int main() {"
                    , "    Int x = -5;"
                    , "    requirePositive(x);"
                    , "    return 0;"
                    , "}"
                    ]
            (errors, unknowns) <- runAnalyzer (parseOk' input)
            
            unknowns `shouldBe` [] -- Nothing is deferred to runtime
            errors `shouldBe` ["test-input:4:5 - Precondition Failed in: requirePositive: x > 0"]