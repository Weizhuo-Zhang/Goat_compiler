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
-- Note: Please filter the StringTable before using this function
--       Cannot be used for String
-- Return the BaseType of given ExpressionTable
-------------------------------------------------------------------------------
getExpressionTableBaseType :: ExpressionTable -> BaseType
getExpressionTableBaseType exprTable =
  case exprTable of
    VariableTable _ exprType -> exprType
    BoolTable  _             -> BoolType
    IntTable   _             -> IntType
    FloatTable _             -> FloatType
    AddTable   _ _  exprType -> exprType
    SubTable   _ _  exprType -> exprType
    MulTable   _ _  exprType -> exprType
    DivTable   _ _  exprType -> exprType
    OrTable    _ _  exprType -> exprType
    AndTable   _ _  exprType -> exprType
    EqTable    _ _  exprType -> exprType
    NotEqTable _ _  exprType -> exprType
    LesTable   _ _  exprType -> exprType
    LesEqTable _ _  exprType -> exprType
    GrtTable   _ _  exprType -> exprType
    GrtEqTable _ _  exprType -> exprType
    NegativeTable _ exprType -> exprType
    NotTable      _ exprType -> exprType

-------------------------------------------------------------------------------
-- Given name of the procedure, which expressions table belongs to, and the
-- left and right expression table, return the expression table's output type.
-------------------------------------------------------------------------------
getExpressionTableType ::
  Identifier -> ExpressionTable -> ExpressionTable -> Either (IO Task) BaseType
getExpressionTableType procName lExprTable rExprTable = do
  case lExprTable of
    IntTable _ -> do
      case rExprTable of
        IntTable _               -> Right IntType
        FloatTable _             -> Right FloatType
        VariableTable _ rVarType -> Right rVarType
        AddTable _ _ rAddType    -> Right rAddType
        SubTable _ _ rSubType    -> Right rSubType
        MulTable _ _ rMulType    -> Right rMulType
        DivTable _ _ rDivType    -> Right rDivType
        NegativeTable _ rVarType -> Right rVarType
        otherwise                -> Left $ exitWithTypeError procName
    FloatTable _ -> Right FloatType
    VariableTable _ lVarType -> getSubExpressionTableType
                                procName
                                lVarType
                                rExprTable
    AddTable    _ _ lAddType -> getSubExpressionTableType
                                procName
                                lAddType
                                rExprTable
    SubTable    _ _ lSubType -> getSubExpressionTableType
                                procName
                                lSubType
                                rExprTable
    MulTable    _ _ lMulType -> getSubExpressionTableType
                                procName
                                lMulType
                                rExprTable
    DivTable    _ _ lDivType -> getSubExpressionTableType
                                procName
                                lDivType
                                rExprTable
    NegativeTable _ lVarType -> getSubExpressionTableType
                                procName
                                lVarType
                                rExprTable
    otherwise                -> Left $ exitWithTypeError procName

-------------------------------------------------------------------------------
-- Given the base type and expression table, return the expression table's
-- output type.
-------------------------------------------------------------------------------
getSubExpressionTableType ::
  Identifier -> BaseType -> ExpressionTable -> Either (IO Task) BaseType
getSubExpressionTableType procName baseType expressionTable = do
  case expressionTable of
    IntTable _               -> Right baseType
    FloatTable _             -> Right FloatType
    VariableTable _ rVarType -> chooseType procName baseType rVarType
    AddTable _ _ rAddType    -> chooseType procName baseType rAddType
    SubTable _ _ rSubType    -> chooseType procName baseType rSubType
    MulTable _ _ rMulType    -> chooseType procName baseType rMulType
    DivTable _ _ rDivType    -> chooseType procName baseType rDivType
    NegativeTable _ rVarType -> chooseType procName baseType rVarType
    otherwise                -> Left $ exitWithTypeError procName

-------------------------------------------------------------------------------
-- Check if there is bool type found, if so, exit with type error message,
-- otherwise return the proper base type.
-------------------------------------------------------------------------------
chooseType :: Identifier -> BaseType -> BaseType -> Either (IO Task) BaseType
chooseType _ IntType IntType   = Right IntType
chooseType _ FloatType _       = Right FloatType
chooseType _ _ FloatType       = Right FloatType
chooseType procName BoolType _ = Left $ exitWithTypeError procName
chooseType procName _ BoolType = Left $ exitWithTypeError procName

-------------------------------------------------------------------------------
-- lookup parameter Map, It must have a base type
-------------------------------------------------------------------------------
lookupBaseTypeParamMap ::
  Identifier -> M.Map Identifier (Int, Parameter) -> BaseType
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
