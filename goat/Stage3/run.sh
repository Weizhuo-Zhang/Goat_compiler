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

echo "----------- asgWithNumericOperatorOnly.gt ----------"
./Goat -s ./testCase/asgWithNumericOperatorOnly.gt > asgWithNumericOperatorOnly.symtable
./Goat ./testCase/asgWithNumericOperatorOnly.gt
