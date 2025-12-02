{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import CodeGen
import LLVM

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Type as T

import Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as BS

import LLVM.Context
import LLVM.Module
import qualified LLVM.AST as AST

import Control.Monad.Except
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

import qualified AbsBasic as B
import LexBasic
import ParBasic
import ErrM

-------------------------------------------------------------------------------
-- Command Line Options
-------------------------------------------------------------------------------

data Options = Options
  { optInput      :: Maybe FilePath
  , optOutput     :: Maybe FilePath
  , optEmitLLVM   :: Bool
  , optHelp       :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optInput      = Nothing
  , optOutput     = Nothing
  , optEmitLLVM   = False
  , optHelp       = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o'] ["output"]
      (ReqArg (\f opts -> opts { optOutput = Just f }) "FILE")
      "Output file (default: out.ll or a.out)"
  , Option ['S'] ["emit-llvm"]
      (NoArg (\opts -> opts { optEmitLLVM = True }))
      "Emit LLVM IR (.ll file)"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help message"
  ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: basic [OPTIONS] <input.basic>"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (opts, files) <- parseOpts args
  
  when (optHelp opts) $ do
    putStrLn banner
    putStrLn $ usageInfo "\nBASIC Compiler - Classic BASIC to LLVM\n\nUsage: basic [OPTIONS] <input.basic>" options
    putStrLn "\nExamples:"
    putStrLn "  basic -S hello.basic              # Generate LLVM IR"
    putStrLn "  basic -S -o out.ll program.basic  # Specify output file"
    putStrLn "  clang out.ll runtime.c -o prog    # Compile to executable"
    putStrLn "\nPart of ACC (Anton's Compiler Collection)"
    putStrLn "Repository: https://github.com/afeldman/basic-compiler"
    exitSuccess
  
  case files of
    [] -> do
      hPutStrLn stderr "Error: No input file specified"
      hPutStrLn stderr $ usageInfo "Usage: basic [OPTIONS] <input.basic>" options
      exitFailure
    
    (file:_) -> do
      source <- readFile file
      
      -- Parse
      case pBasic (myLexer source) of
        Bad err -> do
          hPutStrLn stderr $ "Parse error: " ++ err
          exitFailure
        
        Ok prog -> do
          -- Generate LLVM IR
          let modl = emptyModule "basic"
          newast <- codegen modl prog
          
          if optEmitLLVM opts
            then do
              -- Emit LLVM IR
              let outputFile = case optOutput opts of
                    Just f -> f
                    Nothing -> replaceExtension file ".ll"
              
              llvmIR <- withContext $ \ctx ->
                withModuleFromAST ctx newast moduleLLVMAssembly
              
              BS.writeFile outputFile llvmIR
              putStrLn $ "✓ LLVM IR written to: " ++ outputFile
              putStrLn $ "  Next: clang " ++ outputFile ++ " runtime.c -o " ++ replaceExtension file ""
            
            else do
              -- Compile to object file (would need LLVM compilation)
              let outputFile = case optOutput opts of
                    Just f -> f
                    Nothing -> "a.out"
              
              -- For now, just output LLVM IR
              llvmIR <- withContext $ \ctx ->
                withModuleFromAST ctx newast moduleLLVMAssembly
              
              let llFile = replaceExtension file ".ll"
              BS.writeFile llFile llvmIR
              putStrLn $ "✓ LLVM IR written to: " ++ llFile
              putStrLn $ "  Next: clang " ++ llFile ++ " runtime.c -o " ++ outputFile

-- ASCII Art Banner
banner :: String
banner = unlines
  [ "╔══════════════════════════════════════════════════════════╗"
  , "║                   BASIC COMPILER                         ║"
  , "║              Classic BASIC → LLVM IR                     ║"
  , "║                                                          ║"
  , "║  Features: LET, PRINT, INPUT, GOTO, FOR/NEXT, IF/THEN   ║"
  , "║  Backend:  LLVM IR → Native Code                        ║"
  , "║  Part of:  ACC (Anton's Compiler Collection)            ║"
  , "╚══════════════════════════════════════════════════════════╝"
  ]

-- Helper function
replaceExtension :: FilePath -> String -> FilePath
replaceExtension path newExt = 
  let (base, _) = break (== '.') path
  in base ++ newExt

import Control.Monad (when)
