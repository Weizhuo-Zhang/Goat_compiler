module AnalyzerUtil where

import qualified Data.Map.Strict as M
import           Data.Maybe
import           GoatAST
import           GoatConstant
import           GoatExit
import           SymbolTable
import           Util

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the analyzer util codes.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------------------------------------------------------
-- lookup parameter Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeParamMap :: Identifier -> M.Map Identifier (Int, Parameter) -> BaseType
lookupBaseTypeParamMap varName paramMap =
    case M.lookup varName paramMap of
      Just parameter -> passingType (snd parameter)

-------------------------------------------------------------------------------
-- lookup variable Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeVarMap ::
  Identifier -> M.Map Identifier VariableDeclaration -> BaseType
lookupBaseTypeVarMap varName varMap =
      case M.lookup varName varMap of
        Just variable -> declarationType variable

-------------------------------------------------------------------------------
-- Get procedure identifier from procedure.
-------------------------------------------------------------------------------
getProcedureIdentifier :: Procedure -> Identifier
getProcedureIdentifier = headerIdent . header

-------------------------------------------------------------------------------
-- Get parameters list from procedure.
-------------------------------------------------------------------------------
getProcedureParameters :: Procedure -> [Parameter]
getProcedureParameters = parameters . header

-------------------------------------------------------------------------------
-- Get the error message for multiple declarations for one variable.
-------------------------------------------------------------------------------
getMultipleVarDeclarationErrorMessage :: Identifier -> Identifier -> String
getMultipleVarDeclarationErrorMessage varName procName =
  "There are multiple variable declaration named " ++
  (wrapWithDoubleQuotations varName) ++
  " in procedure " ++
  (wrapWithDoubleQuotations procName)

-------------------------------------------------------------------------------
-- Exit the program with error message indicating multiple declaration for one
-- variable.
-------------------------------------------------------------------------------
exitWithMultipleVarDeclaration :: Identifier -> Identifier -> IO Task
exitWithMultipleVarDeclaration varName procName =
  exitWithError
  (getMultipleVarDeclarationErrorMessage varName procName)
  MultipleVar

-------------------------------------------------------------------------------
-- Get the error message for condition type error.
-------------------------------------------------------------------------------
getConditionTypeErrorMessage :: Identifier -> String
getConditionTypeErrorMessage procName =
  "If condition type error! The type must be bool. In procedure " ++
  (wrapWithDoubleQuotations procName)

-------------------------------------------------------------------------------
-- Exit the program with error message indicating condition type error.
-------------------------------------------------------------------------------
exitWithConditionTypeError :: Identifier -> IO Task
exitWithConditionTypeError procName =
  exitWithError
  (getConditionTypeErrorMessage procName)
  IfCondError

getLogicExprTypeErrorMessage :: Identifier -> String -> String
getLogicExprTypeErrorMessage procName operator =
  operatorWithQuotationMarks ++
  " type error! The argument of " ++
  operatorWithQuotationMarks ++
  " must be bool. In procedure " ++
  (wrapWithDoubleQuotations procName)
  where operatorWithQuotationMarks = wrapWithDoubleQuotations operator

exitWithLogicExprTypeError :: Identifier -> String -> IO Task
exitWithLogicExprTypeError procName operator =
  exitWithError
  (getLogicExprTypeErrorMessage procName operator)
  LogicOpTypeError

getUndefinedVariableErrorMessage :: Identifier -> String
getUndefinedVariableErrorMessage varName =
  "There is a undefined variable named " ++
  (wrapWithDoubleQuotations varName) ++
  " in the statement"

exitWithUndefinedVariable :: Identifier -> IO Task
exitWithUndefinedVariable varName =
  exitWithError
  (getUndefinedVariableErrorMessage varName)
  UndefinedVar

exitWithReadIncorrect :: IO Task
exitWithReadIncorrect =
  exitWithError "Cannot read into non-variable" ReadIncorrect

exitWithTypeError :: Identifier -> IO Task
exitWithTypeError procName =
  exitWithError ("There is a Type Error in the Statment in proc: " ++
                (wrapWithDoubleQuotations procName)) UnmatchedType

getProcNotFoundMessage :: Identifier -> String -> String
getProcNotFoundMessage procName calledProcName =
  "Call Statement Error! Called Procedure " ++
  (wrapWithDoubleQuotations calledProcName) ++
  " is not found in procedure " ++
  (wrapWithDoubleQuotations procName)

exitWithProcNotFound :: Identifier -> String -> IO Task
exitWithProcNotFound procName calledProcName =
  exitWithError
  (getProcNotFoundMessage procName calledProcName)
  UndefinedProc

getCallParamLengthDiffMessage :: Identifier -> String -> String
getCallParamLengthDiffMessage procName calledProcId =
  "Call Statement Error! The parameter of called procedure " ++
  (wrapWithDoubleQuotations calledProcId) ++
  " does not match the declaration in procedure " ++
  (wrapWithDoubleQuotations procName)

exitWithCallParamLengthDiff :: Identifier -> String -> IO Task
exitWithCallParamLengthDiff procName calledProcId =
  exitWithError
  (getCallParamLengthDiffMessage procName calledProcId)
  CallParamNotMatch

getComparisonExprTypeErrorMessage :: Identifier -> String -> String
getComparisonExprTypeErrorMessage procName operator =
  operatorWithQuotationMarks ++
  " type error! The argument of " ++
  operatorWithQuotationMarks ++
  " must be base type. In procedure " ++
  (wrapWithDoubleQuotations procName)
  where operatorWithQuotationMarks = wrapWithDoubleQuotations operator

exitWithComparisonTypeError :: Identifier -> String -> IO Task
exitWithComparisonTypeError procName operator =
  exitWithError
  (getComparisonExprTypeErrorMessage procName operator)
  ComparisonError

getNotSameTypeErrorMessage :: Identifier -> String -> String
getNotSameTypeErrorMessage procName operator =
  operatorWithQuotationMarks ++
  " type error! The argument of " ++
  operatorWithQuotationMarks ++
  " must be same base type. In procedure " ++
  (wrapWithDoubleQuotations procName)
  where operatorWithQuotationMarks = wrapWithDoubleQuotations operator

exitWithNotSameTypeError :: Identifier -> String -> IO Task
exitWithNotSameTypeError procName operator =
  exitWithError
  (getNotSameTypeErrorMessage procName operator)
  NotSameTypeError

getUnaryMinusTypeErrorMessage :: Identifier -> String
getUnaryMinusTypeErrorMessage procName =
  unaryMinusString ++
  " type error! The argument of " ++
  unaryMinusString ++
  " must be int or float. In procedure " ++
  (wrapWithDoubleQuotations procName)
  where unaryMinusString = "\"-\" (Unary Minus)"


exitWithUnaryMinusError :: Identifier -> IO Task
exitWithUnaryMinusError procName =
  exitWithError
  (getUnaryMinusTypeErrorMessage procName)
  UnaryMinusError

getAssignTypeErrorMessage :: Identifier -> String -> String
getAssignTypeErrorMessage procName varName =
  "Assign Type Error! The type of " ++
  (wrapWithDoubleQuotations varName) ++
  " in procedure " ++
  (wrapWithDoubleQuotations procName) ++
  " is not match."

exitWithAssignTypeError :: Identifier -> String -> IO Task
exitWithAssignTypeError procName varName =
  exitWithError
  (getAssignTypeErrorMessage procName varName)
  AssignTypeError

getVarIndicatorErrorMessage :: Identifier -> String -> String
getVarIndicatorErrorMessage procName varName =
  "Variable indicator Error! The variable " ++
  (wrapWithDoubleQuotations varName) ++
  " should not be Array or Matrix in procedure " ++
  (wrapWithDoubleQuotations procName)

exitWithVarIndicatorError :: Identifier -> String -> IO Task
exitWithVarIndicatorError procName varName =
  exitWithError
  (getVarIndicatorErrorMessage procName varName)
  VarIndicatorError

getVarIndicatorNotSameMessage :: Identifier -> String -> String
getVarIndicatorNotSameMessage procName varName =
  "Variable indicator Error! The indicator of variable " ++
  (wrapWithDoubleQuotations varName) ++
  " is not same as declaration" ++
  " in procedure " ++
  (wrapWithDoubleQuotations procName)

exitWithVarIndicatorNotSame :: Identifier -> String -> IO Task
exitWithVarIndicatorNotSame procName varName =
  exitWithError
  (getVarIndicatorNotSameMessage procName varName)
  VarIndicatorError

getArrayMatrixIndicatorTypeErrorMessage :: Identifier -> String -> String
getArrayMatrixIndicatorTypeErrorMessage procName varName =
  "Array and Matrix dimension type Error! The dimension of Array and Matrix" ++
  " must be Int. For variable  "++
  (wrapWithDoubleQuotations varName) ++
  " in procedure " ++
  (wrapWithDoubleQuotations procName)

exitArrayMatrixDimensionTypeError :: Identifier -> String -> IO Task
exitArrayMatrixDimensionTypeError procName varName =
  exitWithError
  (getArrayMatrixIndicatorTypeErrorMessage procName varName)
  VarIndicatorError

getDuplicateProcedureErrorMessage :: Identifier -> String
getDuplicateProcedureErrorMessage procName =
  "There are multiple procedures named " ++ (wrapWithDoubleQuotations procName)


exitWithDuplicateProcedure :: Identifier -> IO Task
exitWithDuplicateProcedure procName =
  exitWithError
  (getDuplicateProcedureErrorMessage procName)
  MultipleProc

getInvalidCallExprMessage :: Identifier -> Identifier -> String
getInvalidCallExprMessage procName calledProcId =
  "Invalid Call statement arguments! The argument of call statement for " ++
  "procedure " ++
  (wrapWithDoubleQuotations calledProcId) ++
  " should only be variable in procedure " ++
  (wrapWithDoubleQuotations procName)

exitWithInvalidCallExpr :: Identifier -> Identifier -> IO Task
exitWithInvalidCallExpr procName calledProcId =
  exitWithError
  (getInvalidCallExprMessage procName calledProcId)
  MultipleProc
