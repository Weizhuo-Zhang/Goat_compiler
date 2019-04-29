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

echo "---------------------- rule 10 ---------------------"
./Goat -p ../testCase/rule_10.gt > ./testCaseOutput/rule_10.txt
diff ../testCase/rule_10_expecting_output.txt ./testCaseOutput/rule_10.txt

echo "---------------------- rule 11 ---------------------"
./Goat -p ../testCase/rule_11.gt > ./testCaseOutput/rule_11.txt
diff ../testCase/rule_11_expecting_output.txt ./testCaseOutput/rule_11.txt

echo "--------------------- comments ---------------------"
./Goat -p ../testCase/comments.gt > ./testCaseOutput/comments.txt
diff ../testCase/comments_expecting_output.txt ./testCaseOutput/comments.txt

echo "------------- expression with brackets -------------"
./Goat -p ../testCase/expression_with_brackets.gt > ./testCaseOutput/expression_with_brackets.txt
diff ../testCase/expression_with_brackets_expecting_output.txt ./testCaseOutput/expression_with_brackets.txt

echo "---------------- matrix whitespace -----------------"
./Goat -p ../testCase/matrix_whitespace.gt > ./testCaseOutput/matrix_whitespace.txt
diff ../testCase/matrix_whitespace_expecting_output.txt ./testCaseOutput/matrix_whitespace.txt

echo "-------------------- assoc.gt ----------------------"
./Goat -p ../testCase/assoc.gt > ./testCaseOutput/output_assoc.txt
diff ../testCase/assoc.out ./testCaseOutput/output_assoc.txt

echo "-------------------- bell.gt -----------------------"
./Goat -p ../testCase/bell.gt > ./testCaseOutput/output_bell.txt
diff ../testCase/bell.out ./testCaseOutput/output_bell.txt

echo "--------------------- gcd.gt -----------------------"
./Goat -p ../testCase/gcd.gt > ./testCaseOutput/output_gcd.txt
diff ../testCase/gcd.out ./testCaseOutput/output_gcd.txt

echo "--------------------- hail.gt ----------------------"
./Goat -p ../testCase/hail.gt > ./testCaseOutput/output_hail.txt
diff ../testCase/hail.out ./testCaseOutput/output_hail.txt

echo "------------------ matrixmul.gt --------------------"
./Goat -p ../testCase/matrixmul.gt > ./testCaseOutput/output_matrixmul.txt
diff ../testCase/matrixmul.out ./testCaseOutput/output_matrixmul.txt

echo "--------------- missing_rel.bad.gt -----------------"
./Goat -p ../testCase/missing_rel.bad.gt > ./testCaseOutput/output_missing_rel.bad.txt
diff ../testCase/missing_rel.bad.out ./testCaseOutput/output_missing_rel.bad.txt

echo "------------------ mode1.bad.gt --------------------"
./Goat -p ../testCase/mode1.bad.gt > ./testCaseOutput/output_mode1.bad.txt
diff ../testCase/mode1.bad.out ./testCaseOutput/output_mode1.bad.txt

echo "------------------ mode2.bad.gt --------------------"
./Goat -p ../testCase/mode2.bad.gt > ./testCaseOutput/output_mode2.bad.txt
diff ../testCase/mode2.bad.out ./testCaseOutput/output_mode2.bad.txt

echo "------------------ mode3.bad.gt --------------------"
./Goat -p ../testCase/mode3.bad.gt > ./testCaseOutput/output_mode3.bad.txt
diff ../testCase/mode3.bad.out ./testCaseOutput/output_mode3.bad.txt

echo "------------------ mode4.bad.gt --------------------"
./Goat -p ../testCase/mode4.bad.gt > ./testCaseOutput/output_mode4.bad.txt
diff ../testCase/mode4.bad.out ./testCaseOutput/output_mode4.bad.txt

echo "---------------- multivar1.bad.gt ------------------"
./Goat -p ../testCase/multivar1.bad.gt > ./testCaseOutput/output_multivar1.bad.txt
diff ../testCase/multivar1.bad.out ./testCaseOutput/output_multivar1.bad.txt

echo "-------------------- power.gt ----------------------"
./Goat -p ../testCase/power.gt > ./testCaseOutput/output_power.txt
diff ../testCase/power.out ./testCaseOutput/output_power.txt

echo "--------------------- q1.gt ------------------------"
./Goat -p ../testCase/q1.gt > ./testCaseOutput/output_q1.txt
diff ../testCase/q1.out ./testCaseOutput/output_q1.txt

echo "--------------------- q2.gt ------------------------"
./Goat -p ../testCase/q2.gt > ./testCaseOutput/output_q2.txt
diff ../testCase/q2.out ./testCaseOutput/output_q2.txt

echo "--------------------- q3.gt ------------------------"
./Goat -p ../testCase/q3.gt > ./testCaseOutput/output_q3.txt
diff ../testCase/q3.out ./testCaseOutput/output_q3.txt

echo "--------------------- q4.gt ------------------------"
./Goat -p ../testCase/q4.gt > ./testCaseOutput/output_q4.txt
diff ../testCase/q4.out ./testCaseOutput/output_q4.txt

echo "-------------------- sort.gt -----------------------"
./Goat -p ../testCase/sort.gt > ./testCaseOutput/output_sort.txt
diff ../testCase/sort.out ./testCaseOutput/output_sort.txt

echo "------------------- stddev.gt ----------------------"
./Goat -p ../testCase/stddev.gt > ./testCaseOutput/output_stddev.txt
diff ../testCase/stddev.out ./testCaseOutput/output_stddev.txt
