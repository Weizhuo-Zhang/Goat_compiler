module GoatExit where

import System.Exit

data Task = Unit | Exit | Compile | Pprint | Parse deriving Eq

-----------------------------------------------------------------
-- Exit code constant, it starts from 0, increase by 1.
-----------------------------------------------------------------
data GoatExitCode = Success       -- 0
                  | MissingFile   -- 1
                  | WrongUsage    -- 2
                  | MissingMain   -- 3
                  | MultipleMain  -- 4
                  | MainWithParam -- 5
                  | ParseError    -- 6
                  deriving Enum


-- Exit message constant

exitWithSuccess :: String -> IO ()
exitWithSuccess message = do
  putStrLn (message)
  exitWith ExitSuccess

-----------------------------------------------------------------
-- print error message to stderr and exit
-----------------------------------------------------------------
exitWithError :: String -> GoatExitCode -> IO Task
exitWithError message exitCode = do
    putStrLn ("[ERROR] " ++ message)
    exitWith (ExitFailure $ fromEnum exitCode)
