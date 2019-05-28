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
./oz ./testCase/GeneratedOutput/ifwhile.oz > ./testCase/GeneratedOutput/ifwhile.txt
diff ./testCase/GeneratedOutput/ifwhile.txt ./testCase/ExpectedOutput/ifwhile.txt

echo "------------------ comparison.gt -------------------"
./Goat testCase/comparison.gt > ./testCase/GeneratedOutput/comparison.oz
./oz ./testCase/GeneratedOutput/comparison.oz > ./testCase/GeneratedOutput/comparison.txt
diff ./testCase/GeneratedOutput/comparison.txt ./testCase/ExpectedOutput/comparison.txt

echo "---------------- comparisonNotEq.gt ----------------"
./Goat testCase/comparisonNotEq.gt > ./testCase/GeneratedOutput/comparisonNotEq.oz
./oz ./testCase/GeneratedOutput/comparisonNotEq.oz > ./testCase/GeneratedOutput/comparisonNotEq.txt
diff ./testCase/GeneratedOutput/comparisonNotEq.txt ./testCase/ExpectedOutput/comparisonNotEq.txt

echo "----------------- comparisonLes.gt -----------------"
./Goat testCase/comparisonLes.gt > ./testCase/GeneratedOutput/comparisonLes.oz
./oz ./testCase/GeneratedOutput/comparisonLes.oz > ./testCase/GeneratedOutput/comparisonLes.txt
diff ./testCase/GeneratedOutput/comparisonLes.txt ./testCase/ExpectedOutput/comparisonLes.txt

echo "--------------- comparisonLesEq.gt -----------------"
./Goat testCase/comparisonLesEq.gt > ./testCase/GeneratedOutput/comparisonLesEq.oz
./oz ./testCase/GeneratedOutput/comparisonLesEq.oz > ./testCase/GeneratedOutput/comparisonLesEq.txt
diff ./testCase/GeneratedOutput/comparisonLesEq.txt ./testCase/ExpectedOutput/comparisonLesEq.txt

echo "------------------ comparisonGrt.gt ----------------"
./Goat testCase/comparisonGrt.gt > ./testCase/GeneratedOutput/comparisonGrt.oz
./oz ./testCase/GeneratedOutput/comparisonGrt.oz > ./testCase/GeneratedOutput/comparisonGrt.txt
diff ./testCase/GeneratedOutput/comparisonGrt.txt ./testCase/ExpectedOutput/comparisonGrt.txt

echo "----------------- comparisonGrtEq.gt ---------------"
./Goat testCase/comparisonGrtEq.gt > ./testCase/GeneratedOutput/comparisonGrtEq.oz
./oz ./testCase/GeneratedOutput/comparisonGrtEq.oz > ./testCase/GeneratedOutput/comparisonGrtEq.txt
diff ./testCase/GeneratedOutput/comparisonGrtEq.txt ./testCase/ExpectedOutput/comparisonGrtEq.txt

echo "----------- asgWithNumericOperatorOnly.gt ----------"
./Goat ./testCase/asgWithNumericOperatorOnly.gt > ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz
./oz ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.oz > ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.txt
diff ./testCase/GeneratedOutput/asgWithNumericOperatorOnly.txt ./testCase/ExpectedOutput/asgWithNumericOperatorOnly.txt


echo "---------- multipleVarDeclaration.out.gt -----------"
./Goat ./testCase/multipleVarDeclaration.out.gt > ./testCase/GeneratedOutput/multipleVarDeclaration.out.oz
diff ./testCase/GeneratedOutput/multipleVarDeclaration.out.oz ./testCase/ExpectedOutput/multipleVarDeclaration.out.oz

echo "------------------ proccall.gt ---------------------"
./Goat ./testCase/proccall.gt > ./testCase/GeneratedOutput/proccall.oz
./oz ./testCase/GeneratedOutput/proccall.oz > ./testCase/GeneratedOutput/proccall.txt
diff ./testCase/GeneratedOutput/proccall.txt ./testCase/ExpectedOutput/proccall.txt


# echo "------------------------- read.gt -------------------------"
# ./Goat ./testCase/read.gt > ./testCase/GeneratedOutput/read.oz
# ./oz ./testCase/GeneratedOutput/read.oz


echo "\n\n----------------- Test by folders ------------------\n"

echo "--------------------- Miles folder... ---------------------\n"
echo "------------------------- Mile 1 --------------------------"
./Goat ./testCase/miles/mile1.gt > ./testCase/GeneratedOutput/miles/mile1.oz
./oz ./testCase/GeneratedOutput/miles/mile1.oz > ./testCase/GeneratedOutput/miles/mile1.txt
diff ./testCase/GeneratedOutput/miles/mile1.txt ./testCase/ExpectedOutput/mile1.txt

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
./oz ./testCase/GeneratedOutput/visible/asg.oz > ./testCase/GeneratedOutput/visible/asg.txt
diff ./testCase/GeneratedOutput/visible/asg.txt ./testCase/ExpectedOutput/asg.txt

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

echo "-------------------- Visible folder... --------------------\n"
echo "------------------------- aadornobenit.gt -------------------------"
./Goat ./testCase/peers/aadornobenit.gt > ./testCase/GeneratedOutput/peers/aadornobenit.oz
./oz ./testCase/GeneratedOutput/peers/aadornobenit.oz

echo "------------------------- alanu.gt -------------------------"
./Goat ./testCase/peers/alanu.gt > ./testCase/GeneratedOutput/peers/alanu.oz
./oz ./testCase/GeneratedOutput/peers/alanu.oz

echo "------------------------- aluo1.gt -------------------------"
./Goat ./testCase/peers/aluo1.gt > ./testCase/GeneratedOutput/peers/aluo1.oz
./oz ./testCase/GeneratedOutput/peers/aluo1.oz

echo "------------------------- apurushotha.gt -------------------------"
./Goat ./testCase/peers/apurushotha.gt > ./testCase/GeneratedOutput/peers/apurushotha.oz
./oz ./testCase/GeneratedOutput/peers/apurushotha.oz

echo "------------------------- austinl.bad.gt -------------------------"
./Goat ./testCase/peers/austinl.bad.gt > ./testCase/GeneratedOutput/peers/austinl.bad.oz
./oz ./testCase/GeneratedOutput/peers/austinl.bad.oz

echo "------------------------- bgooding.gt -------------------------"
./Goat ./testCase/peers/bgooding.gt > ./testCase/GeneratedOutput/peers/bgooding.oz
./oz ./testCase/GeneratedOutput/peers/bgooding.oz

echo "------------------------- chenqinz.gt -------------------------"
./Goat ./testCase/peers/chenqinz.gt > ./testCase/GeneratedOutput/peers/chenqinz.oz
./oz ./testCase/GeneratedOutput/peers/chenqinz.oz

echo "------------------------- chunyaow.gt -------------------------"
./Goat ./testCase/peers/chunyaow.gt > ./testCase/GeneratedOutput/peers/chunyaow.oz
./oz ./testCase/GeneratedOutput/peers/chunyaow.oz

echo "------------------------- codya.gt -------------------------"
./Goat ./testCase/peers/codya.gt > ./testCase/GeneratedOutput/peers/codya.oz
./oz ./testCase/GeneratedOutput/peers/codya.oz

echo "------------------------- dbarrell.gt -------------------------"
./Goat ./testCase/peers/dbarrell.gt > ./testCase/GeneratedOutput/peers/dbarrell.oz
./oz ./testCase/GeneratedOutput/peers/dbarrell.oz

echo "------------------------- dstern.gt -------------------------"
./Goat ./testCase/peers/dstern.gt > ./testCase/GeneratedOutput/peers/dstern.oz
./oz ./testCase/GeneratedOutput/peers/dstern.oz

echo "------------------------- farrugiam.gt -------------------------"
./Goat ./testCase/peers/farrugiam.gt > ./testCase/GeneratedOutput/peers/farrugiam.oz
./oz ./testCase/GeneratedOutput/peers/farrugiam.oz

echo "------------------------- glaw.gt -------------------------"
./Goat ./testCase/peers/glaw.gt > ./testCase/GeneratedOutput/peers/glaw.oz
./oz ./testCase/GeneratedOutput/peers/glaw.oz

echo "------------------------- guoenj.gt -------------------------"
./Goat ./testCase/peers/guoenj.gt > ./testCase/GeneratedOutput/peers/guoenj.oz
./oz ./testCase/GeneratedOutput/peers/guoenj.oz

echo "------------------------- haohail.runbad.gt -------------------------"
./Goat ./testCase/peers/haohail.runbad.gt > ./testCase/GeneratedOutput/peers/haohail.runbad.oz
./oz ./testCase/GeneratedOutput/peers/haohail.runbad.oz

echo "------------------------- harald.gt -------------------------"
./Goat ./testCase/peers/harald.gt > ./testCase/GeneratedOutput/peers/harald.oz
./oz ./testCase/GeneratedOutput/peers/harald.oz

echo "------------------------- jiaxingt.gt -------------------------"
./Goat ./testCase/peers/jiaxingt.gt > ./testCase/GeneratedOutput/peers/jiaxingt.oz
./oz ./testCase/GeneratedOutput/peers/jiaxingt.oz

echo "------------------------- liangy6.gt -------------------------"
./Goat ./testCase/peers/liangy6.gt > ./testCase/GeneratedOutput/peers/liangy6.oz
./oz ./testCase/GeneratedOutput/peers/liangy6.oz

echo "------------------------- linghanz1.gt -------------------------"
./Goat ./testCase/peers/linghanz1.gt > ./testCase/GeneratedOutput/peers/linghanz1.oz
./oz ./testCase/GeneratedOutput/peers/linghanz1.oz

echo "------------------------- liyaoz.gt -------------------------"
./Goat ./testCase/peers/liyaoz.gt > ./testCase/GeneratedOutput/peers/liyaoz.oz
./oz ./testCase/GeneratedOutput/peers/liyaoz.oz

echo "------------------------- lpendock.gt -------------------------"
./Goat ./testCase/peers/lpendock.gt > ./testCase/GeneratedOutput/peers/lpendock.oz
./oz ./testCase/GeneratedOutput/peers/lpendock.oz

echo "------------------------- mchan.gt -------------------------"
./Goat ./testCase/peers/mchan.gt > ./testCase/GeneratedOutput/peers/mchan.oz
./oz ./testCase/GeneratedOutput/peers/mchan.oz

echo "------------------------- mingdam.gt -------------------------"
./Goat ./testCase/peers/mingdam.gt > ./testCase/GeneratedOutput/peers/mingdam.oz
./oz ./testCase/GeneratedOutput/peers/mingdam.oz

echo "------------------------- mingyangz.gt -------------------------"
./Goat ./testCase/peers/mingyangz.gt > ./testCase/GeneratedOutput/peers/mingyangz.oz
./oz ./testCase/GeneratedOutput/peers/mingyangz.oz

echo "------------------------- minjianc.gt -------------------------"
./Goat ./testCase/peers/minjianc.gt > ./testCase/GeneratedOutput/peers/minjianc.oz
./oz ./testCase/GeneratedOutput/peers/minjianc.oz

echo "------------------------- peiyuh.gt -------------------------"
./Goat ./testCase/peers/peiyuh.gt > ./testCase/GeneratedOutput/peers/peiyuh.oz
./oz ./testCase/GeneratedOutput/peers/peiyuh.oz

echo "------------------------- raymonds1.gt -------------------------"
./Goat ./testCase/peers/raymonds1.gt > ./testCase/GeneratedOutput/peers/raymonds1.oz
./oz ./testCase/GeneratedOutput/peers/raymonds1.oz

echo "------------------------- shijiel2.gt -------------------------"
./Goat ./testCase/peers/shijiel2.gt > ./testCase/GeneratedOutput/peers/shijiel2.oz
./oz ./testCase/GeneratedOutput/peers/shijiel2.oz

echo "------------------------- shizhec.gt -------------------------"
./Goat ./testCase/peers/shizhec.gt > ./testCase/GeneratedOutput/peers/shizhec.oz
./oz ./testCase/GeneratedOutput/peers/shizhec.oz

echo "------------------------- shumaox.gt -------------------------"
./Goat ./testCase/peers/shumaox.gt > ./testCase/GeneratedOutput/peers/shumaox.oz
./oz ./testCase/GeneratedOutput/peers/shumaox.oz

echo "------------------------- stevent2.gt -------------------------"
./Goat ./testCase/peers/stevent2.gt > ./testCase/GeneratedOutput/peers/stevent2.oz
./oz ./testCase/GeneratedOutput/peers/stevent2.oz

echo "------------------------- weizhix.gt -------------------------"
./Goat ./testCase/peers/weizhix.gt > ./testCase/GeneratedOutput/peers/weizhix.oz
./oz ./testCase/GeneratedOutput/peers/weizhix.oz

echo "------------------------- weizhuoz.gt -------------------------"
./Goat ./testCase/peers/weizhuoz.gt > ./testCase/GeneratedOutput/peers/weizhuoz.oz
./oz ./testCase/GeneratedOutput/peers/weizhuoz.oz

echo "------------------------- wenqingx.gt -------------------------"
./Goat ./testCase/peers/wenqingx.gt > ./testCase/GeneratedOutput/peers/wenqingx.oz
./oz ./testCase/GeneratedOutput/peers/wenqingx.oz

echo "------------------------- wentaol4.gt -------------------------"
./Goat ./testCase/peers/wentaol4.gt > ./testCase/GeneratedOutput/peers/wentaol4.oz
./oz ./testCase/GeneratedOutput/peers/wentaol4.oz

echo "------------------------- wleong3.gt -------------------------"
./Goat ./testCase/peers/wleong3.gt > ./testCase/GeneratedOutput/peers/wleong3.oz
./oz ./testCase/GeneratedOutput/peers/wleong3.oz

echo "------------------------- yigew3.gt -------------------------"
./Goat ./testCase/peers/yigew3.gt > ./testCase/GeneratedOutput/peers/yigew3.oz
./oz ./testCase/GeneratedOutput/peers/yigew3.oz

echo "------------------------- yiqunw1.bad.gt -------------------------"
./Goat ./testCase/peers/yiqunw1.bad.gt > ./testCase/GeneratedOutput/peers/yiqunw1.bad.oz
./oz ./testCase/GeneratedOutput/peers/yiqunw1.bad.oz

echo "------------------------- yixiong.gt -------------------------"
./Goat ./testCase/peers/yixiong.gt > ./testCase/GeneratedOutput/peers/yixiong.oz
./oz ./testCase/GeneratedOutput/peers/yixiong.oz

echo "------------------------- yiyue.gt -------------------------"
./Goat ./testCase/peers/yiyue.gt > ./testCase/GeneratedOutput/peers/yiyue.oz
./oz ./testCase/GeneratedOutput/peers/yiyue.oz

echo "------------------------- yuankail.gt -------------------------"
./Goat ./testCase/peers/yuankail.gt > ./testCase/GeneratedOutput/peers/yuankail.oz
./oz ./testCase/GeneratedOutput/peers/yuankail.oz

echo "------------------------- zeleic.gt -------------------------"
./Goat ./testCase/peers/zeleic.gt > ./testCase/GeneratedOutput/peers/zeleic.oz
./oz ./testCase/GeneratedOutput/peers/zeleic.oz

echo "------------------------- zeyuh3.gt -------------------------"
./Goat ./testCase/peers/zeyuh3.gt > ./testCase/GeneratedOutput/peers/zeyuh3.oz
./oz ./testCase/GeneratedOutput/peers/zeyuh3.oz

echo "------------------------- zhengjiex1.gt -------------------------"
./Goat ./testCase/peers/zhengjiex1.gt > ./testCase/GeneratedOutput/peers/zhengjiex1.oz
./oz ./testCase/GeneratedOutput/peers/zhengjiex1.oz

echo "------------------------- zhet1.gt -------------------------"
./Goat ./testCase/peers/zhet1.gt > ./testCase/GeneratedOutput/peers/zhet1.oz
./oz ./testCase/GeneratedOutput/peers/zhet1.oz

echo "------------------------- zijunc3.gt -------------------------"
./Goat ./testCase/peers/zijunc3.gt > ./testCase/GeneratedOutput/peers/zijunc3.oz
./oz ./testCase/GeneratedOutput/peers/zijunc3.oz

make clean
