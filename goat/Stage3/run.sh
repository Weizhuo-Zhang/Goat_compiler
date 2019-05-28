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

echo "--------------------- hello.gt ---------------------"
./Goat testCase/hello.gt > ./testCase/GeneratedOutput/hello.oz
diff ./testCase/GeneratedOutput/hello.oz ./testCase/ExpectedOutput/hello.oz
./oz ./testCase/GeneratedOutput/hello.oz

echo "----------- ifwhile.gt ----------"
./Goat testCase/ifwhile.gt > ./testCase/GeneratedOutput/ifwhile.oz
diff ./testCase/GeneratedOutput/ifwhile.oz ./testCase/ExpectedOutput/ifwhile.oz
./oz ./testCase/GeneratedOutput/ifwhile.oz

echo "----------- comparison.gt ----------"
./Goat testCase/comparison.gt > ./testCase/GeneratedOutput/comparison.oz
./oz ./testCase/GeneratedOutput/comparison.oz

echo "----------- comparisonNotEq.gt ----------"
./Goat testCase/comparisonNotEq.gt > ./testCase/GeneratedOutput/comparisonNotEq.oz
./oz ./testCase/GeneratedOutput/comparisonNotEq.oz

echo "----------- comparisonLes.gt ----------"
./Goat testCase/comparisonLes.gt > ./testCase/GeneratedOutput/comparisonLes.oz
./oz ./testCase/GeneratedOutput/comparisonLes.oz

echo "----------- comparisonLesEq.gt ----------"
./Goat testCase/comparisonLesEq.gt > ./testCase/GeneratedOutput/comparisonLesEq.oz
./oz ./testCase/GeneratedOutput/comparisonLesEq.oz

echo "----------- comparisonGrt.gt ----------"
./Goat testCase/comparisonGrt.gt > ./testCase/GeneratedOutput/comparisonGrt.oz
./oz ./testCase/GeneratedOutput/comparisonGrt.oz

echo "----------- comparisonGrtEq.gt ----------"
./Goat testCase/comparisonGrtEq.gt > ./testCase/GeneratedOutput/comparisonGrtEq.oz
./oz ./testCase/GeneratedOutput/comparisonGrtEq.oz

echo "------ asgWithNumericOperatorOnly.gt ----"
./Goat ./testCase/asgWithNumericOperatorOnly.gt > ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz
./oz ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz

echo "---------------- asg.gt -----------------"
./Goat ./testCase/asg.gt > ./testCase/GeneratedOutput/asg.oz
./oz ./testCase/GeneratedOutput/asg.oz

echo "---------------- arrayprod.gt -----------------"
./Goat ./testCase/arrayprod.gt > ./testCase/GeneratedOutput/arrayprod.oz
./oz ./testCase/GeneratedOutput/arrayprod.oz

echo "---------------- assoc.gt -----------------"
./Goat ./testCase/assoc.gt > ./testCase/GeneratedOutput/assoc.oz
./oz ./testCase/GeneratedOutput/assoc.oz

echo "---------------- bell.gt -----------------"
./Goat ./testCase/bell.gt > ./testCase/GeneratedOutput/bell.oz
./oz ./testCase/GeneratedOutput/bell.oz

echo "---------------- hail.gt -----------------"
./Goat ./testCase/hail.gt > ./testCase/GeneratedOutput/hail.oz
./oz ./testCase/GeneratedOutput/hail.oz

echo "---------------- matrixmul.gt -----------------"
./Goat ./testCase/matrixmul.gt > ./testCase/GeneratedOutput/matrixmul.oz
./oz ./testCase/GeneratedOutput/matrixmul.oz

echo "---------------- mile1.gt -----------------"
./Goat ./testCase/mile1.gt > ./testCase/GeneratedOutput/mile1.oz
./oz ./testCase/GeneratedOutput/mile1.oz

echo "---------------- mile3.gt -----------------"
./Goat ./testCase/mile3.gt > ./testCase/GeneratedOutput/mile3.oz
./oz ./testCase/GeneratedOutput/mile3.oz

echo "---------------- mile4.gt -----------------"
./Goat ./testCase/mile4.gt > ./testCase/GeneratedOutput/mile4.oz
./oz ./testCase/GeneratedOutput/mile4.oz

echo "---------------- mile5.gt -----------------"
./Goat ./testCase/mile5.gt > ./testCase/GeneratedOutput/mile5.oz
./oz ./testCase/GeneratedOutput/mile5.oz

echo "---------------- multipleVarDeclaration.gt -----------------"
./Goat ./testCase/multipleVarDeclaration.gt > ./testCase/GeneratedOutput/multipleVarDeclaration.oz
./oz ./testCase/GeneratedOutput/multipleVarDeclaration.oz

echo "---------------- power.gt -----------------"
./Goat ./testCase/power.gt > ./testCase/GeneratedOutput/power.oz
./oz ./testCase/GeneratedOutput/power.oz

echo "---------------- proccall.gt -----------------"
./Goat ./testCase/proccall.gt > ./testCase/GeneratedOutput/proccall.oz
./oz ./testCase/GeneratedOutput/proccall.oz

echo "---------------- q46.gt -----------------"
./Goat ./testCase/q46.gt > ./testCase/GeneratedOutput/q46.oz
./oz ./testCase/GeneratedOutput/q46.oz

echo "---------------- q48.gt -----------------"
./Goat ./testCase/q48.gt > ./testCase/GeneratedOutput/q48.oz
./oz ./testCase/GeneratedOutput/q48.oz

echo "---------------- q49.gt -----------------"
./Goat ./testCase/q49.gt > ./testCase/GeneratedOutput/q49.oz
./oz ./testCase/GeneratedOutput/q49.oz

echo "---------------- q50.gt -----------------"
./Goat ./testCase/q50.gt > ./testCase/GeneratedOutput/q50.oz
./oz ./testCase/GeneratedOutput/q50.oz

echo "---------------- q51.gt -----------------"
./Goat ./testCase/q51.gt > ./testCase/GeneratedOutput/q51.oz
./oz ./testCase/GeneratedOutput/q51.oz

echo "\n\n-------------------- Start of I/O test --------------------"

echo "---------------- read.gt ----------------"
./Goat ./testCase/read.gt > ./testCase/GeneratedOutput/read.oz
./oz ./testCase/GeneratedOutput/read.oz

echo "---------------- gcd.gt -----------------"
./Goat ./testCase/gcd.gt > ./testCase/GeneratedOutput/gcd.oz
./oz ./testCase/GeneratedOutput/gcd.oz

echo "---------------- io.gt -----------------"
./Goat ./testCase/io.gt > ./testCase/GeneratedOutput/io.oz
./oz ./testCase/GeneratedOutput/io.oz

echo "---------------- mile2.gt -----------------"
./Goat ./testCase/mile2.gt > ./testCase/GeneratedOutput/mile2.oz
./oz ./testCase/GeneratedOutput/mile2.oz

echo "---------------- mile6.gt -----------------"
./Goat ./testCase/mile6.gt > ./testCase/GeneratedOutput/mile6.oz
./oz ./testCase/GeneratedOutput/mile6.oz

echo "---------------- q45.gt -----------------"
./Goat ./testCase/q45.gt > ./testCase/GeneratedOutput/q45.oz
./oz ./testCase/GeneratedOutput/q45.oz

echo "---------------- sort.gt -----------------"
./Goat ./testCase/sort.gt > ./testCase/GeneratedOutput/sort.oz
./oz ./testCase/GeneratedOutput/sort.oz
echo "--------------------- End of I/O test ---------------------\n"

make clean
