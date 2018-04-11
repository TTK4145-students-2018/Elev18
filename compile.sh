#Compile script taken from Sivertba's ErlangHeis2016
erlc -v *.erl;
mkdir ebin;
mv *.beam ebin/;
cd ebin/;
erl
