#Compile script taken from Sivertba's ErlangHeis2016
erlc *.erl;
mkdir ebin;
mv *.beam ebin/;
cd ebin/;
erl
