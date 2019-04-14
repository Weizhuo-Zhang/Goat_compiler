make clean
ghc Goat.hs -o Goat
./Goat -p testCase/blank.gt
./Goat -p testCase/2_hello.gt
./Goat -p testCase/test.gt
./Goat -p testCase/hello_para_in_main.gt
./Goat -p testCase/asg.gt
./Goat -p testCase/hello.gt
./Goat -p testCase/io.gt
./Goat -p testCase/test_hello.gt
./Goat -p testCase/stmtTest.gt
./Goat -p testCase/backslash.gt
./Goat -p testCase/write_test.gt

# testing for matrix
./Goat -p testCase/matrix_whitespace.gt > testCase/matrix_whitespace_output.txt
diff testCase/matrix_whitespace_expecting_output.txt testCase/matrix_whitespace_output.txt
