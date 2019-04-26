##############################################################################
#
# How to use?
#   To use this shell script, copy our testCase folder to the same folder 
#   where the anonymous peer review codes are, copy this file into the peer
#   review codes folder, create a folder called testCaseOutput. After that,
#   run the script using `bash ./Stage2Run.sh` or `zsh ./Stage2Run.sh`, based
#   on teh shell you are using.
#
##############################################################################

make

echo "---------------------- rule 1 ----------------------"
./Goat -p ../testCase/rule1.gt > ./testCaseOutput/rule1.txt
diff ../testCase/rule1_out.txt ./testCaseOutput/rule1.txt

echo "---------------------- rule 2 ----------------------"
./Goat -p ../testCase/rule2.gt > ./testCaseOutput/rule2.txt
diff ../testCase/rule2_out.txt ./testCaseOutput/rule2.txt

echo "---------------------- rule 3 ----------------------"
./Goat -p ../testCase/rule3.gt > ./testCaseOutput/rule3.txt
diff ../testCase/rule3_out.txt ./testCaseOutput/rule3.txt

echo "---------------------- rule 4 ----------------------"
./Goat -p ../testCase/rule4.gt > ./testCaseOutput/rule4.txt
diff ../testCase/rule4_out.txt ./testCaseOutput/rule4.txt

echo "---------------------- rule 6 ----------------------"
./Goat -p ../testCase/rule6.gt > ./testCaseOutput/rule6.txt
diff ../testCase/rule6_out.txt ./testCaseOutput/rule6.txt

echo "---------------------- rule 7 ----------------------"
./Goat -p ../testCase/rule7.gt > ./testCaseOutput/rule7.txt
diff ../testCase/rule7_out.txt ./testCaseOutput/rule7.txt

echo "---------------------- rule 8 ----------------------"
./Goat -p ../testCase/rule8.gt > ./testCaseOutput/rule8.txt
diff ../testCase/rule8_out.txt ./testCaseOutput/rule8.txt

echo "---------------------- rule 9 ----------------------"
./Goat -p ../testCase/rule_9.gt > ./testCaseOutput/rule_9.txt
diff ../testCase/rule_9_expecting_output.txt ./testCaseOutput/rule_9.txt

echo "---------------------- rule 10 ----------------------"
./Goat -p ../testCase/rule_10.gt > ./testCaseOutput/rule_10.txt
diff ../testCase/rule_10_expecting_output.txt ./testCaseOutput/rule_10.txt

echo "---------------------- rule 11 ----------------------"
./Goat -p ../testCase/rule_11.gt > ./testCaseOutput/rule_11.txt
diff ../testCase/rule_11_expecting_output.txt ./testCaseOutput/rule_11.txt

echo "---------------------- comments ----------------------"
./Goat -p ../testCase/comments.gt > ./testCaseOutput/comments.txt
diff ../testCase/comments_expecting_output.txt ./testCaseOutput/comments.txt

echo "---------------------- expression with brackets ----------------------"
./Goat -p ../testCase/expression_with_brackets.gt > ./testCaseOutput/expression_with_brackets.txt
diff ../testCase/expression_with_brackets_expecting_output.txt ./testCaseOutput/expression_with_brackets.txt

echo "---------------------- matrix whitespace ----------------------"
./Goat -p ../testCase/matrix_whitespace.gt > ./testCaseOutput/matrix_whitespace.txt
diff ../testCase/matrix_whitespace_expecting_output.txt ./testCaseOutput/matrix_whitespace.txt
