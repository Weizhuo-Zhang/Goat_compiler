module GoatExit where

import System.Exit

-------------------------------- Documentation --------------------------------

-- Authors:
--   Shizhe Cai (shizhec) - 798125
--   Weizhuo Zhang (weizhuoz) - 1018329
--   Mingyang Zhang (mingyangz) - 650242
--   An Luo (aluo1) - 657605

-- This file contains the exit-related information of the Goat program,
-- including 2 data types: ExitCode and Task, and 2 functions: exitWithSuccess
-- and exitWithError.

-- The aim of the project is to implement a compiler for a procedural (C-like)
-- language called Goat.

-------------------------------- Documentation --------------------------------

-------------------------------------------------------------------------------
-- Task type of this Goat compiler.
-------------------------------------------------------------------------------
data Task = Unit | Exit | Compile | Pprint | Parse deriving Eq

-------------------------------------------------------------------------------
-- Exit code constant, it starts from 0, increase by 1.
-------------------------------------------------------------------------------
data GoatExitCode = Success       -- 0
                  | MissingFile   -- 1
                  | WrongUsage    -- 2
                  | MissingMain   -- 3
                  | MultipleMain  -- 4
                  | MainWithParam -- 5
                  | ParseError    -- 6
                  deriving Enum

-------------------------------------------------------------------------------
-- Print message to stdout, and exit as successful.
-------------------------------------------------------------------------------
exitWithSuccess :: String -> IO ()
exitWithSuccess message = do
  putStrLn (message)
  exitWith ExitSuccess

-------------------------------------------------------------------------------
-- Print error message to stderr and exit
-------------------------------------------------------------------------------
exitWithError :: String -> GoatExitCode -> IO Task
exitWithError message exitCode = do
    putStrLn ("[ERROR] " ++ message)
    exitWith (ExitFailure $ fromEnum exitCode)