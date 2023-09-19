# Řešení: FLP Logický projekt - Rubikova kostka
# Autor: Radek Duchoň, xducho07
# Datum: 29. 4. 2021

all:
	swipl -q -g start -o flp20-log -c proj.pl

clean:
	rm flp20-log

test:
	time ./flp20-log < tests/1-1
	time ./flp20-log < tests/2-1
	time ./flp20-log < tests/3-1
	time ./flp20-log < tests/4-1
	time ./flp20-log < tests/4-2
	time ./flp20-log < tests/4-3
	time ./flp20-log < tests/4-4
	time ./flp20-log < tests/4-5
	time ./flp20-log < tests/4-6
	time ./flp20-log < tests/4-7
	time ./flp20-log < tests/4-8
	time ./flp20-log < tests/4-9
	time ./flp20-log < tests/4-10
	time ./flp20-log < tests/4-11
	time ./flp20-log < tests/4-12
	time ./flp20-log < tests/4-13
	time ./flp20-log < tests/5-1
	time ./flp20-log < tests/5-2
	time ./flp20-log < tests/5-3

zip:
	zip flp-log-xducho07.zip tests/* proj.pl README Makefile
