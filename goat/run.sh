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

# Generate the expecting output for sample test cases.
# ./Goat -p ./sampleTestCase/assoc.gt > ./sampleTestCase/assoc.out
# ./Goat -p ./sampleTestCase/bell.gt > ./sampleTestCase/bell.out
# ./Goat -p ./sampleTestCase/gcd.gt > ./sampleTestCase/gcd.out
# ./Goat -p ./sampleTestCase/hail.gt > ./sampleTestCase/hail.out
# ./Goat -p ./sampleTestCase/matrixmul.gt > ./sampleTestCase/matrixmul.out
# ./Goat -p ./sampleTestCase/missing_rel.bad.gt > ./sampleTestCase/missing_rel.bad.out
# ./Goat -p ./sampleTestCase/mode1.bad.gt > ./sampleTestCase/mode1.bad.out
# ./Goat -p ./sampleTestCase/mode2.bad.gt > ./sampleTestCase/mode2.bad.out
# ./Goat -p ./sampleTestCase/mode3.bad.gt > ./sampleTestCase/mode3.bad.out
# ./Goat -p ./sampleTestCase/mode4.bad.gt > ./sampleTestCase/mode4.bad.out
# ./Goat -p ./sampleTestCase/multivar1.bad.gt > ./sampleTestCase/multivar1.bad.out
# ./Goat -p ./sampleTestCase/power.gt > ./sampleTestCase/power.out
# ./Goat -p ./sampleTestCase/q1.gt > ./sampleTestCase/q1.out
# ./Goat -p ./sampleTestCase/q2.gt > ./sampleTestCase/q2.out
# ./Goat -p ./sampleTestCase/q3.gt > ./sampleTestCase/q3.out
# ./Goat -p ./sampleTestCase/q4.gt > ./sampleTestCase/q4.out
# ./Goat -p ./sampleTestCase/sort.gt > ./sampleTestCase/sort.out
# ./Goat -p ./sampleTestCase/stddev.gt > ./sampleTestCase/stddev.out
