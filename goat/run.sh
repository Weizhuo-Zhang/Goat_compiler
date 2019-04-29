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


echo "---------------------- rule 1 ----------------------"
./Goat -p ./testCase/rule1.gt > ./testCase/output_rule1.txt
diff ./testCase/rule1_out.txt ./testCase/output_rule1.txt

echo "---------------------- rule 2 ----------------------"
./Goat -p ./testCase/rule2.gt > ./testCase/output_rule2.txt
diff ./testCase/rule2_out.txt ./testCase/output_rule2.txt

echo "---------------------- rule 3 ----------------------"
./Goat -p ./testCase/rule3.gt > ./testCase/output_rule3.txt
diff ./testCase/rule3_out.txt ./testCase/output_rule3.txt

echo "---------------------- rule 4 ----------------------"
./Goat -p ./testCase/rule4.gt > ./testCase/output_rule4.txt
diff ./testCase/rule4_out.txt ./testCase/output_rule4.txt

echo "---------------------- rule 6 ----------------------"
./Goat -p ./testCase/rule6.gt > ./testCase/output_rule6.txt
diff ./testCase/rule6_out.txt ./testCase/output_rule6.txt

echo "---------------------- rule 7 ----------------------"
./Goat -p ./testCase/rule7.gt > ./testCase/output_rule7.txt
diff ./testCase/rule7_out.txt ./testCase/output_rule7.txt

echo "---------------------- rule 8 ----------------------"
./Goat -p ./testCase/rule8.gt > ./testCase/output_rule8.txt
diff ./testCase/rule8_out.txt ./testCase/output_rule8.txt

echo "---------------------- rule 9 ----------------------"
./Goat -p ./testCase/rule_9.gt > ./testCase/output_rule_9.txt
diff ./testCase/rule_9_expecting_output.txt ./testCase/output_rule_9.txt

echo "---------------------- rule 10 ---------------------"
./Goat -p ./testCase/rule_10.gt > ./testCase/output_rule_10.txt
diff ./testCase/rule_10_expecting_output.txt ./testCase/output_rule_10.txt

echo "---------------------- rule 11 ---------------------"
./Goat -p ./testCase/rule_11.gt > ./testCase/output_rule_11.txt
diff ./testCase/rule_11_expecting_output.txt ./testCase/output_rule_11.txt

echo "--------------------- comments ---------------------"
./Goat -p ./testCase/comments.gt > ./testCase/output_comments.txt
diff ./testCase/comments_expecting_output.txt ./testCase/output_comments.txt

echo "------------- expression with brackets -------------"
./Goat -p ./testCase/expression_with_brackets.gt > ./testCase/output_expression_with_brackets.txt
diff ./testCase/expression_with_brackets_expecting_output.txt ./testCase/output_expression_with_brackets.txt

echo "---------------- matrix whitespace -----------------"
./Goat -p ./testCase/matrix_whitespace.gt > ./testCase/output_matrix_whitespace.txt
diff ./testCase/matrix_whitespace_expecting_output.txt ./testCase/output_matrix_whitespace.txt

echo "-------------------- assoc.gt ----------------------"
./Goat -p ./testCase/assoc.gt > ./testCase/output_assoc.txt
diff ./testCase/assoc.out ./testCase/output_assoc.txt

echo "-------------------- bell.gt -----------------------"
./Goat -p ./testCase/bell.gt > ./testCase/output_bell.txt
diff ./testCase/bell.out ./testCase/output_bell.txt

echo "--------------------- gcd.gt -----------------------"
./Goat -p ./testCase/gcd.gt > ./testCase/output_gcd.txt
diff ./testCase/gcd.out ./testCase/output_gcd.txt

echo "--------------------- hail.gt ----------------------"
./Goat -p ./testCase/hail.gt > ./testCase/output_hail.txt
diff ./testCase/hail.out ./testCase/output_hail.txt

echo "------------------ matrixmul.gt --------------------"
./Goat -p ./testCase/matrixmul.gt > ./testCase/output_matrixmul.txt
diff ./testCase/matrixmul.out ./testCase/output_matrixmul.txt

echo "--------------- missing_rel.bad.gt -----------------"
./Goat -p ./testCase/missing_rel.bad.gt > ./testCase/output_missing_rel.bad.txt
diff ./testCase/missing_rel.bad.out ./testCase/output_missing_rel.bad.txt

echo "------------------ mode1.bad.gt --------------------"
./Goat -p ./testCase/mode1.bad.gt > ./testCase/output_mode1.bad.txt
diff ./testCase/mode1.bad.out ./testCase/output_mode1.bad.txt

echo "------------------ mode2.bad.gt --------------------"
./Goat -p ./testCase/mode2.bad.gt > ./testCase/output_mode2.bad.txt
diff ./testCase/mode2.bad.out ./testCase/output_mode2.bad.txt

echo "------------------ mode3.bad.gt --------------------"
./Goat -p ./testCase/mode3.bad.gt > ./testCase/output_mode3.bad.txt
diff ./testCase/mode3.bad.out ./testCase/output_mode3.bad.txt

echo "------------------ mode4.bad.gt --------------------"
./Goat -p ./testCase/mode4.bad.gt > ./testCase/output_mode4.bad.txt
diff ./testCase/mode4.bad.out ./testCase/output_mode4.bad.txt

echo "---------------- multivar1.bad.gt ------------------"
./Goat -p ./testCase/multivar1.bad.gt > ./testCase/output_multivar1.bad.txt
diff ./testCase/multivar1.bad.out ./testCase/output_multivar1.bad.txt

echo "-------------------- power.gt ----------------------"
./Goat -p ./testCase/power.gt > ./testCase/output_power.txt
diff ./testCase/power.out ./testCase/output_power.txt

echo "--------------------- q1.gt ------------------------"
./Goat -p ./testCase/q1.gt > ./testCase/output_q1.txt
diff ./testCase/q1.out ./testCase/output_q1.txt

echo "--------------------- q2.gt ------------------------"
./Goat -p ./testCase/q2.gt > ./testCase/output_q2.txt
diff ./testCase/q2.out ./testCase/output_q2.txt

echo "--------------------- q3.gt ------------------------"
./Goat -p ./testCase/q3.gt > ./testCase/output_q3.txt
diff ./testCase/q3.out ./testCase/output_q3.txt

echo "--------------------- q4.gt ------------------------"
./Goat -p ./testCase/q4.gt > ./testCase/output_q4.txt
diff ./testCase/q4.out ./testCase/output_q4.txt

echo "-------------------- sort.gt -----------------------"
./Goat -p ./testCase/sort.gt > ./testCase/output_sort.txt
diff ./testCase/sort.out ./testCase/output_sort.txt

echo "------------------- stddev.gt ----------------------"
./Goat -p ./testCase/stddev.gt > ./testCase/output_stddev.txt
diff ./testCase/stddev.out ./testCase/output_stddev.txt
