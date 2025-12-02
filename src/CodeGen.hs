{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Control.Monad.State
import Control.Applicative

import qualified AbsBasic as B
import LLVM as L

-------------------------------------------------------------------------------
-- Code Generation
-------------------------------------------------------------------------------

-- BASIC uses line numbers as labels
type LineMap = Map.Map Integer Name

data BasicState = BasicState
  { lineMap :: LineMap
  , variables :: Map.Map String Operand
  , forLoops :: [(String, Operand, Operand, Operand, Name)]  -- var, end, step, counter, loopStart
  }

emptyBasicState :: BasicState
emptyBasicState = BasicState Map.empty Map.empty []

type BasicGen = StateT BasicState Codegen

-- Generate LLVM Module from BASIC program
codegenTop :: B.Basic -> LLVM ()
codegenTop prog = do
  -- External declarations for runtime functions
  external L.voidType "print_double" [(L.double, Name "x")]
  external L.voidType "print_string" [(L.ptrType L.i8, Name "s")]
  external L.double "input_double" []
  external L.double "read_string_as_double" [(L.ptrType L.i8, Name "s")]
  
  -- Main function
  define L.i32 "main" [] $ execCodegen $ flip evalStateT emptyBasicState $ do
    entry <- lift $ addBlock entryBlockName
    lift $ setBlock entry
    
    -- Generate code for program
    cgenBasic prog
    
    -- Return 0
    lift $ ret (L.iconst 0)

cgenBasic :: B.Basic -> BasicGen ()
cgenBasic (B.BasicGr lines) = do
  -- First pass: create labels for all line numbers
  forM_ lines $ \(B.LBasic lineNum _) -> do
    label <- lift $ addBlock ("line" ++ show lineNum)
    modify $ \s -> s { lineMap = Map.insert lineNum label (lineMap s) }
  
  -- Second pass: generate code
  forM_ lines $ \line -> cgenLine line
  
cgenLine :: B.Line -> BasicGen ()
cgenLine (B.LBasic lineNum stm) = do
  label <- gets (Map.lookup lineNum . lineMap)
  case label of
    Just lbl -> do
      lift $ setBlock lbl
      cgenStm stm
    Nothing -> error $ "Line number not found: " ++ show lineNum

cgenStm :: B.Stm -> BasicGen ()
cgenStm stmt = case stmt of
  B.SRem str -> return ()  -- Comment, do nothing
  
  B.SGoTo lineNum -> do
    targetLabel <- gets (Map.lookup lineNum . lineMap)
    case targetLabel of
      Just lbl -> lift $ br lbl >> return ()
      Nothing -> error $ "GOTO target not found: " ++ show lineNum
  
  B.SLet (B.Ident var) expr -> do
    val <- cgenExp expr
    -- Allocate or update variable
    varMap <- gets variables
    ptr <- case Map.lookup var varMap of
      Just p -> return p
      Nothing -> do
        p <- lift $ alloca L.double
        modify $ \s -> s { variables = Map.insert var p (variables s) }
        return p
    lift $ store ptr val
  
  B.SPrint expr -> do
    val <- cgenExp expr
    printFunc <- lift $ externf (Name "print_double")
    lift $ call printFunc [val]
    return ()
  
  B.SInput (B.Ident var) -> do
    inputFunc <- lift $ externf (Name "input_double")
    val <- lift $ call inputFunc []
    
    varMap <- gets variables
    ptr <- case Map.lookup var varMap of
      Just p -> return p
      Nothing -> do
        p <- lift $ alloca L.double
        modify $ \s -> s { variables = Map.insert var p (variables s) }
        return p
    lift $ store ptr val
  
  B.SIfStm (B.Ident var) condExp thenExp -> do
    -- Load variable
    varMap <- gets variables
    case Map.lookup var varMap of
      Just ptr -> do
        varVal <- lift $ load L.double ptr
        condVal <- cgenExp condExp
        
        -- Compare
        cond <- lift $ fcmp FP.OEQ varVal condVal
        condBool <- lift $ instr L.i32 $ Trunc cond L.i32 []
        
        -- Create blocks
        thenBlock <- lift $ addBlock "if.then"
        elseBlock <- lift $ addBlock "if.else"
        
        -- Branch
        lift $ cbr condBool thenBlock elseBlock
        
        -- Then block
        lift $ setBlock thenBlock
        _ <- cgenExp thenExp  -- Execute then expression
        lift $ br elseBlock
        
        -- Continue
        lift $ setBlock elseBlock
      Nothing -> error $ "Variable not defined: " ++ var
  
  B.SIfElse (B.Ident var) condExp thenExp elseExp -> do
    varMap <- gets variables
    case Map.lookup var varMap of
      Just ptr -> do
        varVal <- lift $ load L.double ptr
        condVal <- cgenExp condExp
        
        cond <- lift $ fcmp FP.OEQ varVal condVal
        condBool <- lift $ instr L.i32 $ Trunc cond L.i32 []
        
        thenBlock <- lift $ addBlock "if.then"
        elseBlock <- lift $ addBlock "if.else"
        contBlock <- lift $ addBlock "if.cont"
        
        lift $ cbr condBool thenBlock elseBlock
        
        lift $ setBlock thenBlock
        _ <- cgenExp thenExp
        lift $ br contBlock
        
        lift $ setBlock elseBlock
        _ <- cgenExp elseExp
        lift $ br contBlock
        
        lift $ setBlock contBlock
      Nothing -> error $ "Variable not defined: " ++ var
  
  B.SFORStm (B.Ident var) startExp endExp -> do
    -- FOR var = start TO end (STEP 1)
    startVal <- cgenExp startExp
    endVal <- cgenExp endExp
    let stepVal = L.fconst 1.0
    
    -- Initialize loop variable
    varMap <- gets variables
    ptr <- case Map.lookup var varMap of
      Just p -> return p
      Nothing -> do
        p <- lift $ alloca L.double
        modify $ \s -> s { variables = Map.insert var p (variables s) }
        return p
    lift $ store ptr startVal
    
    -- Create loop blocks
    loopCond <- lift $ addBlock ("for." ++ var ++ ".cond")
    loopBody <- lift $ addBlock ("for." ++ var ++ ".body")
    loopEnd <- lift $ addBlock ("for." ++ var ++ ".end")
    
    -- Jump to condition
    lift $ br loopCond
    
    -- Store loop info for NEXT
    modify $ \s -> s { forLoops = (var, endVal, stepVal, ptr, loopCond) : forLoops s }
    
    lift $ setBlock loopCond
    currentVal <- lift $ load L.double ptr
    cond <- lift $ fcmp FP.OLE currentVal endVal
    lift $ cbr cond loopBody loopEnd
    
    lift $ setBlock loopBody
  
  B.SFORStmS (B.Ident var) startExp endExp step -> do
    -- FOR var = start TO end STEP step
    startVal <- cgenExp startExp
    endVal <- cgenExp endExp
    let stepVal = L.fconst (fromIntegral step)
    
    varMap <- gets variables
    ptr <- case Map.lookup var varMap of
      Just p -> return p
      Nothing -> do
        p <- lift $ alloca L.double
        modify $ \s -> s { variables = Map.insert var p (variables s) }
        return p
    lift $ store ptr startVal
    
    loopCond <- lift $ addBlock ("for." ++ var ++ ".cond")
    loopBody <- lift $ addBlock ("for." ++ var ++ ".body")
    loopEnd <- lift $ addBlock ("for." ++ var ++ ".end")
    
    lift $ br loopCond
    
    modify $ \s -> s { forLoops = (var, endVal, stepVal, ptr, loopCond) : forLoops s }
    
    lift $ setBlock loopCond
    currentVal <- lift $ load L.double ptr
    cond <- lift $ fcmp FP.OLE currentVal endVal
    lift $ cbr cond loopBody loopEnd
    
    lift $ setBlock loopBody
  
  B.SNext (B.Ident var) -> do
    loops <- gets forLoops
    case find (\(v,_,_,_,_) -> v == var) loops of
      Just (_, endVal, stepVal, ptr, loopCond) -> do
        -- Increment counter
        currentVal <- lift $ load L.double ptr
        newVal <- lift $ fadd currentVal stepVal
        lift $ store ptr newVal
        
        -- Jump back to condition
        lift $ br loopCond
        
        -- Remove loop from stack
        modify $ \s -> s { forLoops = filter (\(v,_,_,_,_) -> v /= var) (forLoops s) }
      Nothing -> error $ "NEXT without matching FOR: " ++ var
  
  B.SEnd -> do
    lift $ ret (L.iconst 0)
    return ()

cgenExp :: B.Exp -> BasicGen Operand
cgenExp expr = case expr of
  B.EInt n -> return $ L.fconst (fromIntegral n)
  
  B.EString str -> do
    -- For now, strings are converted to 0.0
    -- TODO: Implement proper string handling
    return $ L.fconst 0.0
  
  B.EIdent (B.Ident var) -> do
    varMap <- gets variables
    case Map.lookup var varMap of
      Just ptr -> lift $ load L.double ptr
      Nothing -> error $ "Variable not defined: " ++ var
  
  B.EBreace e -> cgenExp e
  
  B.EOperation e1 op e2 -> do
    v1 <- cgenExp e1
    v2 <- cgenExp e2
    cgenOp op v1 v2
  
  B.EUOperation uop e -> do
    v <- cgenExp e
    cgenUOp uop v

cgenOp :: B.Op -> Operand -> Operand -> BasicGen Operand
cgenOp op v1 v2 = lift $ case op of
  B.OPlus  -> fadd v1 v2
  B.OMinus -> fsub v1 v2
  B.OMult  -> fmul v1 v2
  B.ODiv   -> fdiv v1 v2
  B.OMod   -> frem v1 v2
  B.OEq    -> do
    cmp <- fcmp FP.OEQ v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.OLt    -> do
    cmp <- fcmp FP.OLT v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.OLtEq  -> do
    cmp <- fcmp FP.OLE v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.OGt    -> do
    cmp <- fcmp FP.OGT v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.OGtEq  -> do
    cmp <- fcmp FP.OGE v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.ONEq   -> do
    cmp <- fcmp FP.ONE v1 v2
    instr L.double $ UIToFP cmp L.double []
  B.OAnd   -> fmul v1 v2  -- Simple AND: both non-zero = multiply
  B.OOr    -> fadd v1 v2  -- Simple OR: at least one non-zero = add

cgenUOp :: B.UOp -> Operand -> BasicGen Operand
cgenUOp uop v = lift $ case uop of
  B.UOMinus -> fsub (L.fconst 0.0) v
  B.UONot   -> do
    -- NOT: if v == 0 then 1 else 0
    cmp <- fcmp FP.OEQ v (L.fconst 0.0)
    instr L.double $ UIToFP cmp L.double []

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> B.Basic -> IO AST.Module
codegen modo prog = do
  let modn = runLLVM modo (codegenTop prog)
  return modn
