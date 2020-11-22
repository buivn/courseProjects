default: sumsq par_sumsq check check2 main

sumsq: sumsq.c
	gcc -g -O0 --std=c99 -Wall -o sumsq sumsq.c

par_sumsq: par_sumsq.c
	gcc -g -O0 --std=c99 -Wall -o par_sumsq par_sumsq.c -lpthread

check: check.c
	gcc -g -O0 --std=c99 -Wall -o check check.c -lpthread

check2: check2.c
	gcc -g -O0 --std=c99 -Wall -o check2 check2.c -lpthread

main: main.cpp thread_pool.cpp thread_pool.h
	g++ -o main main.cpp thread_pool.cpp -lpthread

thre_pool1: thre_pool1.c
	gcc -g -O0 --std=c99 -Wall -o thre_pool1 thre_pool1.c -lpthread

par_sumsq_s: par_sumsq_s.c
	gcc -g -O2 --std=c99 -Wall -o par_sumsq_s par_sumsq_s.c -lpthread


clean: 
	rm -f sumsq par_sumsq check main