make clean
ghc Goat.hs -o Goat

echo "\n\n===================== Stage 1 ======================\n"

echo "---------------------- rule 1 ----------------------"
./Goat -p ./testCase/rule1.gt > ./testCase/GeneratedOutput/output_rule1.txt
diff ./testCase/ExpectingOutput/rule1_expecting_output.txt ./testCase/GeneratedOutput/output_rule1.txt

echo "---------------------- rule 2 ----------------------"
./Goat -p ./testCase/rule2.gt > ./testCase/GeneratedOutput/output_rule2.txt
diff ./testCase/ExpectingOutput/rule2_expecting_output.txt ./testCase/GeneratedOutput/output_rule2.txt

echo "---------------------- rule 3 ----------------------"
./Goat -p ./testCase/rule3.gt > ./testCase/GeneratedOutput/output_rule3.txt
diff ./testCase/ExpectingOutput/rule3_expecting_output.txt ./testCase/GeneratedOutput/output_rule3.txt

echo "---------------------- rule 4 ----------------------"
./Goat -p ./testCase/rule4.gt > ./testCase/GeneratedOutput/output_rule4.txt
diff ./testCase/ExpectingOutput/rule4_expecting_output.txt ./testCase/GeneratedOutput/output_rule4.txt

echo "---------------------- rule 6 ----------------------"
./Goat -p ./testCase/rule6.gt > ./testCase/GeneratedOutput/output_rule6.txt
diff ./testCase/ExpectingOutput/rule6_expecting_output.txt ./testCase/GeneratedOutput/output_rule6.txt

echo "---------------------- rule 7 ----------------------"
./Goat -p ./testCase/rule7.gt > ./testCase/GeneratedOutput/output_rule7.txt
diff ./testCase/ExpectingOutput/rule7_expecting_output.txt ./testCase/GeneratedOutput/output_rule7.txt

echo "---------------------- rule 8 ----------------------"
./Goat -p ./testCase/rule8.gt > ./testCase/GeneratedOutput/output_rule8.txt
diff ./testCase/ExpectingOutput/rule8_expecting_output.txt ./testCase/GeneratedOutput/output_rule8.txt

echo "---------------------- rule 9 ----------------------"
./Goat -p ./testCase/rule_9.gt > ./testCase/GeneratedOutput/output_rule_9.txt
diff ./testCase/ExpectingOutput/rule_9_expecting_output.txt ./testCase/GeneratedOutput/output_rule_9.txt

echo "---------------------- rule 10 ---------------------"
./Goat -p ./testCase/rule_10.gt > ./testCase/GeneratedOutput/output_rule_10.txt
diff ./testCase/ExpectingOutput/rule_10_expecting_output.txt ./testCase/GeneratedOutput/output_rule_10.txt

echo "---------------------- rule 11 ---------------------"
./Goat -p ./testCase/rule_11.gt > ./testCase/GeneratedOutput/output_rule_11.txt
diff ./testCase/ExpectingOutput/rule_11_expecting_output.txt ./testCase/GeneratedOutput/output_rule_11.txt

echo "--------------------- comments ---------------------"
./Goat -p ./testCase/comments.gt > ./testCase/GeneratedOutput/output_comments.txt
diff ./testCase/ExpectingOutput/comments_expecting_output.txt ./testCase/GeneratedOutput/output_comments.txt

echo "------------- expression with brackets -------------"
./Goat -p ./testCase/expression_with_brackets.gt > ./testCase/GeneratedOutput/output_expression_with_brackets.txt
diff ./testCase/ExpectingOutput/expression_with_brackets_expecting_output.txt ./testCase/GeneratedOutput/output_expression_with_brackets.txt

echo "---------------- matrix whitespace -----------------"
./Goat -p ./testCase/matrix_whitespace.gt > ./testCase/GeneratedOutput/output_matrix_whitespace.txt
diff ./testCase/ExpectingOutput/matrix_whitespace_expecting_output.txt ./testCase/GeneratedOutput/output_matrix_whitespace.txt

echo "-------------------- assoc.gt ----------------------"
./Goat -p ./testCase/assoc.gt > ./testCase/GeneratedOutput/output_assoc.txt
diff ./testCase/ExpectingOutput/assoc.out ./testCase/GeneratedOutput/output_assoc.txt

echo "-------------------- bell.gt -----------------------"
./Goat -p ./testCase/bell.gt > ./testCase/GeneratedOutput/output_bell.txt
diff ./testCase/ExpectingOutput/bell.out ./testCase/GeneratedOutput/output_bell.txt

echo "--------------------- gcd.gt -----------------------"
./Goat -p ./testCase/gcd.gt > ./testCase/GeneratedOutput/output_gcd.txt
diff ./testCase/ExpectingOutput/gcd.out ./testCase/GeneratedOutput/output_gcd.txt

echo "--------------------- hail.gt ----------------------"
./Goat -p ./testCase/hail.gt > ./testCase/GeneratedOutput/output_hail.txt
diff ./testCase/ExpectingOutput/hail.out ./testCase/GeneratedOutput/output_hail.txt

echo "------------------ matrixmul.gt --------------------"
./Goat -p ./testCase/matrixmul.gt > ./testCase/GeneratedOutput/output_matrixmul.txt
diff ./testCase/ExpectingOutput/matrixmul.out ./testCase/GeneratedOutput/output_matrixmul.txt

echo "--------------- missing_rel.bad.gt -----------------"
./Goat -p ./testCase/missing_rel.bad.gt > ./testCase/GeneratedOutput/output_missing_rel.bad.txt
diff ./testCase/ExpectingOutput/missing_rel.bad.out ./testCase/GeneratedOutput/output_missing_rel.bad.txt

echo "------------------ mode1.bad.gt --------------------"
./Goat -p ./testCase/mode1.bad.gt > ./testCase/GeneratedOutput/output_mode1.bad.txt
diff ./testCase/ExpectingOutput/mode1.bad.out ./testCase/GeneratedOutput/output_mode1.bad.txt

echo "------------------ mode2.bad.gt --------------------"
./Goat -p ./testCase/mode2.bad.gt > ./testCase/GeneratedOutput/output_mode2.bad.txt
diff ./testCase/ExpectingOutput/mode2.bad.out ./testCase/GeneratedOutput/output_mode2.bad.txt

echo "------------------ mode3.bad.gt --------------------"
./Goat -p ./testCase/mode3.bad.gt > ./testCase/GeneratedOutput/output_mode3.bad.txt
diff ./testCase/ExpectingOutput/mode3.bad.out ./testCase/GeneratedOutput/output_mode3.bad.txt

echo "------------------ mode4.bad.gt --------------------"
./Goat -p ./testCase/mode4.bad.gt > ./testCase/GeneratedOutput/output_mode4.bad.txt
diff ./testCase/ExpectingOutput/mode4.bad.out ./testCase/GeneratedOutput/output_mode4.bad.txt

echo "---------------- multivar1.bad.gt ------------------"
./Goat -p ./testCase/multivar1.bad.gt > ./testCase/GeneratedOutput/output_multivar1.bad.txt
diff ./testCase/ExpectingOutput/multivar1.bad.out ./testCase/GeneratedOutput/output_multivar1.bad.txt

echo "-------------------- power.gt ----------------------"
./Goat -p ./testCase/power.gt > ./testCase/GeneratedOutput/output_power.txt
diff ./testCase/ExpectingOutput/power.out ./testCase/GeneratedOutput/output_power.txt

echo "--------------------- q1.gt ------------------------"
./Goat -p ./testCase/q1.gt > ./testCase/GeneratedOutput/output_q1.txt
diff ./testCase/ExpectingOutput/q1.out ./testCase/GeneratedOutput/output_q1.txt

echo "--------------------- q2.gt ------------------------"
./Goat -p ./testCase/q2.gt > ./testCase/GeneratedOutput/output_q2.txt
diff ./testCase/ExpectingOutput/q2.out ./testCase/GeneratedOutput/output_q2.txt

echo "--------------------- q3.gt ------------------------"
./Goat -p ./testCase/q3.gt > ./testCase/GeneratedOutput/output_q3.txt
diff ./testCase/ExpectingOutput/q3.out ./testCase/GeneratedOutput/output_q3.txt

echo "--------------------- q4.gt ------------------------"
./Goat -p ./testCase/q4.gt > ./testCase/GeneratedOutput/output_q4.txt
diff ./testCase/ExpectingOutput/q4.out ./testCase/GeneratedOutput/output_q4.txt

echo "-------------------- sort.gt -----------------------"
./Goat -p ./testCase/sort.gt > ./testCase/GeneratedOutput/output_sort.txt
diff ./testCase/ExpectingOutput/sort.out ./testCase/GeneratedOutput/output_sort.txt

echo "------------------- stddev.gt ----------------------"
./Goat -p ./testCase/stddev.gt > ./testCase/GeneratedOutput/output_stddev.txt
diff ./testCase/ExpectingOutput/stddev.out ./testCase/GeneratedOutput/output_stddev.txt
