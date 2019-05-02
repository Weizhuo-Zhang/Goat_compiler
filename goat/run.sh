make clean
ghc Goat.hs -o Goat

# ./Goat -p testCase/blank.gt
# ./Goat -p testCase/2_hello.gt
# ./Goat -p testCase/test.gt
# ./Goat -p testCase/hello_para_in_main.gt
# ./Goat -p testCase/asg.gt
# ./Goat -p testCase/hello.gt
# ./Goat -p testCase/io.gt
# ./Goat -p testCase/test_hello.gt
# ./Goat -p testCase/stmtTest.gt
# ./Goat -p testCase/backslash.gt
# ./Goat -p testCase/write_test.gt

echo "\n\n===================== Stage 1 ======================\n"

echo "---------------------- rule 1 ----------------------"
./Goat -p ./testCase/Stage1/rule1.gt > ./testCase/Stage1/GeneratedOutput/output_rule1.txt
diff ./testCase/Stage1/ExpectingOutput/rule1_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule1.txt

echo "---------------------- rule 2 ----------------------"
./Goat -p ./testCase/Stage1/rule2.gt > ./testCase/Stage1/GeneratedOutput/output_rule2.txt
diff ./testCase/Stage1/ExpectingOutput/rule2_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule2.txt

echo "---------------------- rule 3 ----------------------"
./Goat -p ./testCase/Stage1/rule3.gt > ./testCase/Stage1/GeneratedOutput/output_rule3.txt
diff ./testCase/Stage1/ExpectingOutput/rule3_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule3.txt

echo "---------------------- rule 4 ----------------------"
./Goat -p ./testCase/Stage1/rule4.gt > ./testCase/Stage1/GeneratedOutput/output_rule4.txt
diff ./testCase/Stage1/ExpectingOutput/rule4_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule4.txt

echo "---------------------- rule 6 ----------------------"
./Goat -p ./testCase/Stage1/rule6.gt > ./testCase/Stage1/GeneratedOutput/output_rule6.txt
diff ./testCase/Stage1/ExpectingOutput/rule6_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule6.txt

echo "---------------------- rule 7 ----------------------"
./Goat -p ./testCase/Stage1/rule7.gt > ./testCase/Stage1/GeneratedOutput/output_rule7.txt
diff ./testCase/Stage1/ExpectingOutput/rule7_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule7.txt

echo "---------------------- rule 8 ----------------------"
./Goat -p ./testCase/Stage1/rule8.gt > ./testCase/Stage1/GeneratedOutput/output_rule8.txt
diff ./testCase/Stage1/ExpectingOutput/rule8_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule8.txt

echo "---------------------- rule 9 ----------------------"
./Goat -p ./testCase/Stage1/rule_9.gt > ./testCase/Stage1/GeneratedOutput/output_rule_9.txt
diff ./testCase/Stage1/ExpectingOutput/rule_9_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule_9.txt

echo "---------------------- rule 10 ---------------------"
./Goat -p ./testCase/Stage1/rule_10.gt > ./testCase/Stage1/GeneratedOutput/output_rule_10.txt
diff ./testCase/Stage1/ExpectingOutput/rule_10_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule_10.txt

echo "---------------------- rule 11 ---------------------"
./Goat -p ./testCase/Stage1/rule_11.gt > ./testCase/Stage1/GeneratedOutput/output_rule_11.txt
diff ./testCase/Stage1/ExpectingOutput/rule_11_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_rule_11.txt

echo "--------------------- comments ---------------------"
./Goat -p ./testCase/Stage1/comments.gt > ./testCase/Stage1/GeneratedOutput/output_comments.txt
diff ./testCase/Stage1/ExpectingOutput/comments_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_comments.txt

echo "------------- expression with brackets -------------"
./Goat -p ./testCase/Stage1/expression_with_brackets.gt > ./testCase/Stage1/GeneratedOutput/output_expression_with_brackets.txt
diff ./testCase/Stage1/ExpectingOutput/expression_with_brackets_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_expression_with_brackets.txt

echo "---------------- matrix whitespace -----------------"
./Goat -p ./testCase/Stage1/matrix_whitespace.gt > ./testCase/Stage1/GeneratedOutput/output_matrix_whitespace.txt
diff ./testCase/Stage1/ExpectingOutput/matrix_whitespace_expecting_output.txt ./testCase/Stage1/GeneratedOutput/output_matrix_whitespace.txt

echo "-------------------- assoc.gt ----------------------"
./Goat -p ./testCase/Stage1/assoc.gt > ./testCase/Stage1/GeneratedOutput/output_assoc.txt
diff ./testCase/Stage1/ExpectingOutput/assoc.out ./testCase/Stage1/GeneratedOutput/output_assoc.txt

echo "-------------------- bell.gt -----------------------"
./Goat -p ./testCase/Stage1/bell.gt > ./testCase/Stage1/GeneratedOutput/output_bell.txt
diff ./testCase/Stage1/ExpectingOutput/bell.out ./testCase/Stage1/GeneratedOutput/output_bell.txt

echo "--------------------- gcd.gt -----------------------"
./Goat -p ./testCase/Stage1/gcd.gt > ./testCase/Stage1/GeneratedOutput/output_gcd.txt
diff ./testCase/Stage1/ExpectingOutput/gcd.out ./testCase/Stage1/GeneratedOutput/output_gcd.txt

echo "--------------------- hail.gt ----------------------"
./Goat -p ./testCase/Stage1/hail.gt > ./testCase/Stage1/GeneratedOutput/output_hail.txt
diff ./testCase/Stage1/ExpectingOutput/hail.out ./testCase/Stage1/GeneratedOutput/output_hail.txt

echo "------------------ matrixmul.gt --------------------"
./Goat -p ./testCase/Stage1/matrixmul.gt > ./testCase/Stage1/GeneratedOutput/output_matrixmul.txt
diff ./testCase/Stage1/ExpectingOutput/matrixmul.out ./testCase/Stage1/GeneratedOutput/output_matrixmul.txt

echo "--------------- missing_rel.bad.gt -----------------"
./Goat -p ./testCase/Stage1/missing_rel.bad.gt > ./testCase/Stage1/GeneratedOutput/output_missing_rel.bad.txt
diff ./testCase/Stage1/ExpectingOutput/missing_rel.bad.out ./testCase/Stage1/GeneratedOutput/output_missing_rel.bad.txt

echo "------------------ mode1.bad.gt --------------------"
./Goat -p ./testCase/Stage1/mode1.bad.gt > ./testCase/Stage1/GeneratedOutput/output_mode1.bad.txt
diff ./testCase/Stage1/ExpectingOutput/mode1.bad.out ./testCase/Stage1/GeneratedOutput/output_mode1.bad.txt

echo "------------------ mode2.bad.gt --------------------"
./Goat -p ./testCase/Stage1/mode2.bad.gt > ./testCase/Stage1/GeneratedOutput/output_mode2.bad.txt
diff ./testCase/Stage1/ExpectingOutput/mode2.bad.out ./testCase/Stage1/GeneratedOutput/output_mode2.bad.txt

echo "------------------ mode3.bad.gt --------------------"
./Goat -p ./testCase/Stage1/mode3.bad.gt > ./testCase/Stage1/GeneratedOutput/output_mode3.bad.txt
diff ./testCase/Stage1/ExpectingOutput/mode3.bad.out ./testCase/Stage1/GeneratedOutput/output_mode3.bad.txt

echo "------------------ mode4.bad.gt --------------------"
./Goat -p ./testCase/Stage1/mode4.bad.gt > ./testCase/Stage1/GeneratedOutput/output_mode4.bad.txt
diff ./testCase/Stage1/ExpectingOutput/mode4.bad.out ./testCase/Stage1/GeneratedOutput/output_mode4.bad.txt

echo "---------------- multivar1.bad.gt ------------------"
./Goat -p ./testCase/Stage1/multivar1.bad.gt > ./testCase/Stage1/GeneratedOutput/output_multivar1.bad.txt
diff ./testCase/Stage1/ExpectingOutput/multivar1.bad.out ./testCase/Stage1/GeneratedOutput/output_multivar1.bad.txt

echo "-------------------- power.gt ----------------------"
./Goat -p ./testCase/Stage1/power.gt > ./testCase/Stage1/GeneratedOutput/output_power.txt
diff ./testCase/Stage1/ExpectingOutput/power.out ./testCase/Stage1/GeneratedOutput/output_power.txt

echo "--------------------- q1.gt ------------------------"
./Goat -p ./testCase/Stage1/q1.gt > ./testCase/Stage1/GeneratedOutput/output_q1.txt
diff ./testCase/Stage1/ExpectingOutput/q1.out ./testCase/Stage1/GeneratedOutput/output_q1.txt

echo "--------------------- q2.gt ------------------------"
./Goat -p ./testCase/Stage1/q2.gt > ./testCase/Stage1/GeneratedOutput/output_q2.txt
diff ./testCase/Stage1/ExpectingOutput/q2.out ./testCase/Stage1/GeneratedOutput/output_q2.txt

echo "--------------------- q3.gt ------------------------"
./Goat -p ./testCase/Stage1/q3.gt > ./testCase/Stage1/GeneratedOutput/output_q3.txt
diff ./testCase/Stage1/ExpectingOutput/q3.out ./testCase/Stage1/GeneratedOutput/output_q3.txt

echo "--------------------- q4.gt ------------------------"
./Goat -p ./testCase/Stage1/q4.gt > ./testCase/Stage1/GeneratedOutput/output_q4.txt
diff ./testCase/Stage1/ExpectingOutput/q4.out ./testCase/Stage1/GeneratedOutput/output_q4.txt

echo "-------------------- sort.gt -----------------------"
./Goat -p ./testCase/Stage1/sort.gt > ./testCase/Stage1/GeneratedOutput/output_sort.txt
diff ./testCase/Stage1/ExpectingOutput/sort.out ./testCase/Stage1/GeneratedOutput/output_sort.txt

echo "------------------- stddev.gt ----------------------"
./Goat -p ./testCase/Stage1/stddev.gt > ./testCase/Stage1/GeneratedOutput/output_stddev.txt
diff ./testCase/Stage1/ExpectingOutput/stddev.out ./testCase/Stage1/GeneratedOutput/output_stddev.txt

echo "\n\n===================== Stage 3 ======================\n"
