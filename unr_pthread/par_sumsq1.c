/*
 * par_sumsq.c
 * CS 446.646 Project 1 (Pthreads)
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <limits.h>
#include <stdbool.h>
#include <unistd.h>


// aggregate variables
long sum = 0;
long odd = 0;
long min = INT_MAX;
long max = INT_MIN;
bool done = false;
long inputNumber;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
// Declaration of thread condition variable 
pthread_cond_t *cond; // = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond1 = PTHREAD_COND_INITIALIZER;
int *idleThread;

// function prototypes
void *calculate_square(void *num);

// update global aggregate variables given a number
void *calculate_square(void *num)
{
  int number = *((int *) num);
  while (1) {
    
    pthread_mutex_lock(&mutex);
    printf("Thread %d: check done condition\n", number);
    pthread_cond_wait(&cond1, &mutex); 
    *(idleThread+number) = 1;
    printf("Check after the conditon \n");
    // calculate the square
    long the_square = inputNumber * inputNumber;
    // let's add this to our (global) sum
    sum += the_square;
    // now we also tabulate some (meaningless) statistics
    if (inputNumber % 2 == 1) {
      // how many of our numbers were odd?
      odd++;
    }
    // what was the smallest one we had to deal with?
    if (inputNumber < min) {
      min = inputNumber;
    }
    // and what was the biggest one?
    if (inputNumber > max) {
      max = inputNumber;
    }
    pthread_mutex_unlock(&mutex);
    sleep(inputNumber);
    pthread_mutex_lock(&mutex);
    *(idleThread+number) = 0;
    pthread_mutex_unlock(&mutex);
  }
  //pthread_exit(NULL);
}

int main(int argc, char* argv[])
{
  // check and parse command line options
  if (argc != 3) {
    printf("Usage: sumsq <infile>\n");
    exit(EXIT_FAILURE);
  }

  char *f2 = argv[2];
  int threadNumber = atoi(f2);
  // printf("the number of threads: %d\n", threadNumber);
  // int threadIdVector[threadNumber];
  pthread_t threadVector[threadNumber];
  cond = (pthread_cond_t*)malloc(threadNumber*sizeof(pthread_cond_t));
  idleThread = (int*)malloc(threadNumber*sizeof(int));
  *idleThread = 0;
  
  for (int i =0; i<threadNumber; i++) {
    pthread_t thread;
    int threadid;
    int *arg = malloc(sizeof(*arg));
    *arg = i;
    threadid = pthread_create(&thread, NULL, calculate_square, arg);
    threadVector[i] = thread;
  }

  // printf("Check the signal the thread condition 111\n"); 
  char *fn = argv[1];
  // load numbers and add them to the queue
  FILE* fin = fopen(fn, "r");
  char action;
  bool assignStatus = false;
  while (fscanf(fin, "%c %ld\n", &action, &inputNumber) == 2) {
    printf("the input number is: %ld \n", inputNumber);
    if (action == 'p') {            // process, do some work
      assignStatus = false;
      
      while (!assignStatus) {
        // find the idle thread and assign the work
        for (int j=0; j< threadNumber; j++) {
          // if this thread is idle
          if (*(idleThread+j) == 0) {
            int check = pthread_cond_signal(&cond1);
            // assign the task done
            assignStatus = true;
            // if (!check) {
            //   printf("Check the thread number %d \n", j);  
            // }
          }
        }
        if (!assignStatus) {
          sleep(1); // if assigning task is not done, sleep 1 second, and find idle task again
          printf("Sleep to wait for a idle thread \n");
        }
      }
    } 
    else if (action == 'w') {     // wait, nothing new happening
      printf("Sleeping at the main function \n");
      sleep(inputNumber); // the program sleep in num seconds
    } 
    else {
      printf("ERROR: Unrecognized action: '%c'\n", action);
      exit(EXIT_FAILURE);
    }
  }
  fclose(fin);

  // bool allThreadIdle = true;
  // while (!done) {
  //   // printf("have the algorithm run inside here \n");
  //   for (int n=0; n<threadNumber; n++ ) {
  //     if (*(idleThread+n) == 1) {
  //       allThreadIdle = false;
  //       printf("have the algorithm run inside here \n");
  //     }
  //   }
  //   if (allThreadIdle) {
  //     done = true; // the queue is exhausted/
  //   }
  //   sleep(1);
  // }

  for (int k =0; k<threadNumber; k++) {
    //ensures the main thread/program won't terminate until all threads done their jobs
    pthread_join(threadVector[k], NULL);
  }
  
  // print results
  printf("%ld %ld %ld %ld\n", sum, odd, min, max);

  
  // clean up and return
  return (EXIT_SUCCESS);
  // exit(0);
}


