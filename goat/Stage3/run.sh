make clean
make

rm ./oz
cd ../../oz
make clean
make
cp ./oz ../goat/Stage3/oz
make clean
cd ../goat/Stage3

echo "\n\n===================== Stage 3 ======================\n"

echo "--------------------- ifwhile.gt -------------------"
./Goat testCase/ifwhile.gt > ./testCase/GeneratedOutput/ifwhile.oz
diff ./testCase/GeneratedOutput/ifwhile.oz ./testCase/ExpectedOutput/ifwhile.oz
./oz ./testCase/GeneratedOutput/ifwhile.oz

echo "--------------------- comparison.gt ---------------------"
./Goat testCase/comparison.gt > ./testCase/GeneratedOutput/comparison.oz
./oz ./testCase/GeneratedOutput/comparison.oz

echo "-------------------- comparisonNotEq.gt -------------------"
./Goat testCase/comparisonNotEq.gt > ./testCase/GeneratedOutput/comparisonNotEq.oz
./oz ./testCase/GeneratedOutput/comparisonNotEq.oz

echo "--------------------- comparisonLes.gt --------------------"
./Goat testCase/comparisonLes.gt > ./testCase/GeneratedOutput/comparisonLes.oz
./oz ./testCase/GeneratedOutput/comparisonLes.oz

echo "------------------- comparisonLesEq.gt --------------------"
./Goat testCase/comparisonLesEq.gt > ./testCase/GeneratedOutput/comparisonLesEq.oz
./oz ./testCase/GeneratedOutput/comparisonLesEq.oz

echo "--------------------- comparisonGrt.gt --------------------"
./Goat testCase/comparisonGrt.gt > ./testCase/GeneratedOutput/comparisonGrt.oz
./oz ./testCase/GeneratedOutput/comparisonGrt.oz

echo "--------------------- comparisonGrtEq.gt ------------------"
./Goat testCase/comparisonGrtEq.gt > ./testCase/GeneratedOutput/comparisonGrtEq.oz
./oz ./testCase/GeneratedOutput/comparisonGrtEq.oz

echo "--------------- asgWithNumericOperatorOnly.gt -------------"
./Goat ./testCase/asgWithNumericOperatorOnly.gt > ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz
./oz ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz

echo "-------------- multipleVarDeclaration.out.gt --------------"
./Goat ./testCase/multipleVarDeclaration.out.gt > ./testCase/GeneratedOutput/multipleVarDeclaration.out.oz
diff ./testCase/GeneratedOutput/multipleVarDeclaration.out.oz ./testCase/ExpectedOutput/multipleVarDeclaration.out.oz

echo "----------------------- proccall.gt -----------------------"
./Goat ./testCase/proccall.gt > ./testCase/GeneratedOutput/proccall.oz
./oz ./testCase/GeneratedOutput/proccall.oz

# echo "------------------------- read.gt -------------------------"
# ./Goat ./testCase/read.gt > ./testCase/GeneratedOutput/read.oz
# ./oz ./testCase/GeneratedOutput/read.oz


echo "--------------------- Test by folders ---------------------\n"

echo "--------------------- Miles folder... ---------------------\n"
echo "------------------------- Mile 1 --------------------------"
./Goat ./testCase/miles/mile1.gt > ./testCase/GeneratedOutput/miles/mile1.oz
./oz ./testCase/GeneratedOutput/miles/mile1.oz

echo "------------------------- Mile 2 --------------------------"
./Goat ./testCase/miles/mile2.gt > ./testCase/GeneratedOutput/miles/mile2.oz
./oz ./testCase/GeneratedOutput/miles/mile2.oz

echo "------------------------- Mile 3 --------------------------"
./Goat ./testCase/miles/mile3.gt > ./testCase/GeneratedOutput/miles/mile3.oz
./oz ./testCase/GeneratedOutput/miles/mile3.oz

echo "------------------------- Mile 4 --------------------------"
./Goat ./testCase/miles/mile4.gt > ./testCase/GeneratedOutput/miles/mile4.oz
./oz ./testCase/GeneratedOutput/miles/mile4.oz

echo "------------------------- Mile 5 --------------------------"
./Goat ./testCase/miles/mile5.gt > ./testCase/GeneratedOutput/miles/mile5.oz
./oz ./testCase/GeneratedOutput/miles/mile5.oz

echo "------------------------- Mile 6 --------------------------"
./Goat ./testCase/miles/mile6.gt > ./testCase/GeneratedOutput/miles/mile6.oz
./oz ./testCase/GeneratedOutput/miles/mile6.oz
echo "------------------- End of Miles folder -------------------\n"

echo "-------------------- Visible folder... --------------------\n"
echo "---------------------- arrayprod.gt -----------------------"
./Goat ./testCase/visible/arrayprod.gt > ./testCase/GeneratedOutput/visible/arrayprod.oz
./oz ./testCase/GeneratedOutput/visible/arrayprod.oz

echo "------------------------- asg.gt --------------------------"
./Goat ./testCase/visible/asg.gt > ./testCase/GeneratedOutput/visible/asg.oz
./oz ./testCase/GeneratedOutput/visible/asg.oz

echo "------------------------ assoc.gt -------------------------"
./Goat ./testCase/visible/assoc.gt > ./testCase/GeneratedOutput/visible/assoc.oz
./oz ./testCase/GeneratedOutput/visible/assoc.oz

echo "------------------------ bell.gt --------------------------"
./Goat ./testCase/visible/bell.gt > ./testCase/GeneratedOutput/visible/bell.oz
./oz ./testCase/GeneratedOutput/visible/bell.oz

echo "------------------------ gcd.gt ---------------------------"
./Goat ./testCase/visible/gcd.gt > ./testCase/GeneratedOutput/visible/gcd.oz
./oz ./testCase/GeneratedOutput/visible/gcd.oz

echo "------------------------- hail.gt -------------------------"
./Goat ./testCase/visible/hail.gt > ./testCase/GeneratedOutput/visible/hail.oz
./oz ./testCase/GeneratedOutput/visible/hail.oz

echo "------------------------- hello.gt ------------------------"
./Goat testCase/visible/hello.gt > ./testCase/GeneratedOutput/visible/hello.oz
./oz ./testCase/GeneratedOutput/visible/hello.oz

echo "-------------------------- io.gt --------------------------"
./Goat ./testCase/visible/io.gt > ./testCase/GeneratedOutput/visible/io.oz
./oz ./testCase/GeneratedOutput/visible/io.oz

echo "-------------------------- matrixmul.gt -------------------"
./Goat ./testCase/visible/matrixmul.gt > ./testCase/GeneratedOutput/visible/matrixmul.oz
./oz ./testCase/GeneratedOutput/visible/matrixmul.oz

echo "------------------------- power.gt ------------------------"
./Goat ./testCase/visible/power.gt > ./testCase/GeneratedOutput/visible/power.oz
./oz ./testCase/GeneratedOutput/visible/power.oz

echo "------------------------- q45.gt --------------------------"
./Goat ./testCase/visible/q45.gt > ./testCase/GeneratedOutput/visible/q45.oz
./oz ./testCase/GeneratedOutput/visible/q45.oz

echo "------------------------- q46.gt --------------------------"
./Goat ./testCase/visible/q46.gt > ./testCase/GeneratedOutput/visible/q46.oz
./oz ./testCase/GeneratedOutput/visible/q46.oz

echo "------------------------- q48.gt --------------------------"
./Goat ./testCase/visible/q48.gt > ./testCase/GeneratedOutput/visible/q48.oz
./oz ./testCase/GeneratedOutput/visible/q48.oz

echo "------------------------- q49.gt --------------------------"
./Goat ./testCase/visible/q49.gt > ./testCase/GeneratedOutput/visible/q49.oz
./oz ./testCase/GeneratedOutput/visible/q49.oz

echo "------------------------- q50.gt --------------------------"
./Goat ./testCase/visible/q50.gt > ./testCase/GeneratedOutput/visible/q50.oz
./oz ./testCase/GeneratedOutput/visible/q50.oz > ./testCase/GeneratedOutput/visible/q50.txt
diff ./testCase/GeneratedOutput/visible/q50.txt ./testCase/ExpectedOutput/q50.txt

echo "------------------------- q51.gt --------------------------"
./Goat ./testCase/visible/q51.gt > ./testCase/GeneratedOutput/visible/q51.oz
./oz ./testCase/GeneratedOutput/visible/q51.oz > ./testCase/GeneratedOutput/visible/q51.txt
diff ./testCase/GeneratedOutput/visible/q51.txt ./testCase/ExpectedOutput/q51.txt

echo "------------------------- sort.gt -------------------------"
./Goat ./testCase/visible/sort.gt > ./testCase/GeneratedOutput/visible/sort.oz
./oz ./testCase/GeneratedOutput/visible/sort.oz

echo "------------------------- stddev.gt -------------------------"
./Goat ./testCase/visible/stddev.gt > ./testCase/GeneratedOutput/visible/stddev.oz
./oz ./testCase/GeneratedOutput/visible/stddev.oz

echo "------------------ End of Visible folder ------------------\n"

make clean
