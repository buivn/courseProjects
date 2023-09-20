#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <limits.h>

// aggregate variables
long sum = 0;
long odd = 0;
long min = INT_MAX;
long max = INT_MIN;
int count_ = 0;

typedef void (*thread_func_t)(void *arg);
// structure to store a task/work
struct tpool_work {
    thread_func_t      func;  // the function to process the data
    void              *arg; // the input data
    struct tpool_work *next;
};
typedef struct tpool_work tpool_work_t;
// a structure to set a list of data will be processed
struct tpool;
struct tpool {
    tpool_work_t    *work_first; // the first data address
    tpool_work_t    *work_last;  // the last data address
    pthread_mutex_t  work_mutex; // only a mutex in the whole program
    pthread_cond_t   work_cond;   // conditional variable to active thread as new data arrive 
    pthread_cond_t   working_cond; // condition variable to manage the active thread
    size_t           working_cnt; // number of active thread
    size_t           thread_cnt;  // number of thread
    bool             stop;        // stop the program
};
typedef struct tpool tpool_t;
// declare list of functions
static tpool_work_t *tpool_work_create(thread_func_t func, void *arg);
static void tpool_work_destroy(tpool_work_t *work);
static tpool_work_t *tpool_work_get(tpool_t *tm);
static void *tpool_worker(void *arg);
void tpool_destroy(tpool_t *tm);
bool tpool_add_work(tpool_t *tm, thread_func_t func, void *arg);
tpool_t *tpool_create(size_t num);
void tpool_wait(tpool_t *tm);
void calculate_square(void *num);

// create a data object
static tpool_work_t *tpool_work_create(thread_func_t func, void *arg) {
  tpool_work_t *work;
  if (func == NULL)
   return NULL;
  work       = malloc(sizeof(*work));
  work->func = func;
  work->arg  = arg;
  work->next = NULL;
  return work;
}

// destroy the data object after completing the task
static void tpool_work_destroy(tpool_work_t *work) {
  if (work == NULL)
    return;
  free(work);
}
// get a data/work from the list
static tpool_work_t *tpool_work_get(tpool_t *tm) {
  tpool_work_t *work;
  if (tm == NULL)
      return NULL;
  work = tm->work_first;
  if (work == NULL)
      return NULL;
  if (work->next == NULL) {
      tm->work_first = NULL;
      tm->work_last  = NULL;
  } else {
      tm->work_first = work->next;
  }
  return work;
}
// function to process a data
static void *tpool_worker(void *arg) {
  tpool_t      *tm = arg;
  tpool_work_t *work;
  while (1) {
      pthread_mutex_lock(&(tm->work_mutex));
      while ((tm->work_first == NULL) && (!tm->stop))
          pthread_cond_wait(&(tm->work_cond), &(tm->work_mutex));
      if (tm->stop) {
        break;
      }
      work = tpool_work_get(tm);
      tm->working_cnt++;
      pthread_mutex_unlock(&(tm->work_mutex));
      if (work != NULL) {
        // work->func(work->arg);
        long number = *((long *) work->arg);
        // calculate the square
        long the_square = number * number;
        sleep(number);
        pthread_mutex_lock(&(tm->work_mutex));
        // let's add this to our (global) sum
        sum += the_square;
        // now we also tabulate some (meaningless) statistics
        if (number % 2 == 1) {
          odd++; // how many of our numbers were odd?
        }
        // what was the smallest one we had to deal with?
        if (number < min) {
          min = number;
        }
        if (number > max) { 
          max = number;
        }
        pthread_mutex_unlock(&(tm->work_mutex));
        tpool_work_destroy(work);
      }
      pthread_mutex_lock(&(tm->work_mutex));
      tm->working_cnt--;
      if (!tm->stop && tm->working_cnt == 0 && tm->work_first == NULL)
          pthread_cond_signal(&(tm->working_cond));
      pthread_mutex_unlock(&(tm->work_mutex));
  }
  tm->thread_cnt--;
  pthread_cond_signal(&(tm->working_cond));
  pthread_mutex_unlock(&(tm->work_mutex));
  return NULL;
}
// function initiliaze the structure list
tpool_t *tpool_create(size_t num) {
    tpool_t   *tm;
    pthread_t  thread;
    size_t     i;
    if (num == 0)
        num = 1;
    tm             = calloc(1, sizeof(*tm));
    tm->thread_cnt = num;
    pthread_mutex_init(&(tm->work_mutex), NULL);
    pthread_cond_init(&(tm->work_cond), NULL);
    pthread_cond_init(&(tm->working_cond), NULL);
    tm->work_first = NULL;
    tm->work_last  = NULL;
    tm->working_cnt=0;
    tm->stop = false;
    for (i=0; i<num; i++) {
        pthread_create(&thread, NULL, tpool_worker, tm);
        pthread_detach(thread);
    }
    return tm;
}
// destroy the structure after completing the task
void tpool_destroy(tpool_t *tm) {
  tpool_work_t *work;
  tpool_work_t *work2;
  if (tm == NULL)
      return;
  pthread_mutex_lock(&(tm->work_mutex));
  work = tm->work_first;
  while (work != NULL) {
      work2 = work->next;
      tpool_work_destroy(work);
      work = work2;
  }
  tm->stop = true;
  pthread_cond_broadcast(&(tm->work_cond));
  pthread_mutex_unlock(&(tm->work_mutex));
  tpool_wait(tm);
  pthread_mutex_destroy(&(tm->work_mutex));
  pthread_cond_destroy(&(tm->work_cond));
  pthread_cond_destroy(&(tm->working_cond));
  free(tm);
}
// function to add a new data/task into the list
bool tpool_add_work(tpool_t *tm, thread_func_t func, void *arg) {
    
    tpool_work_t *work;
    if (tm == NULL)
        return false;
    work = tpool_work_create(func, arg);
    if (work == NULL)
        return false;
    pthread_mutex_lock(&(tm->work_mutex));
    if (tm->work_first == NULL) {
        tm->work_first = work;
        tm->work_last  = tm->work_first;
    } 
    else {
        tm->work_last->next = work;
        tm->work_last       = work;
    }
    pthread_cond_broadcast(&(tm->work_cond));
    pthread_mutex_unlock(&(tm->work_mutex));
    return true;
}
// wait untill all thread are completed
void tpool_wait(tpool_t *tm) {
  if (tm == NULL)
      return;
  pthread_mutex_lock(&(tm->work_mutex));
  while (1) {
      if (count_ < 1) {
        pthread_cond_wait(&(tm->working_cond), &(tm->work_mutex));
      }
      else if ((!tm->stop && tm->working_cnt != 0) || (tm->stop && tm->thread_cnt != 0)) {
        pthread_cond_wait(&(tm->working_cond), &(tm->work_mutex));
      }
      else {
        break;
      }
      count_++;
  }
  pthread_mutex_unlock(&(tm->work_mutex));
}

void calculate_square(void *num) {}

int main(int argc, char **argv)
{
  // check and parse command line options
  if (argc != 3) {
    printf("Usage: sumsq <infile>\n");
    exit(EXIT_FAILURE);
  }
  char *f2 = argv[2];
  int threadNumber = atoi(f2);
  tpool_t *tm;
  tm   = tpool_create(threadNumber); // initialize the thread pool

  char *fn = argv[1];
  // load numbers and add them to the queue
  FILE* fin = fopen(fn, "r");
  char action;
  long inputNumber;
  printf("The processed number list: ");
  while (fscanf(fin, "%c %ld\n", &action, &inputNumber) == 2) {
    if (action == 'p') {            // process, do some work
      long *arg = malloc(sizeof(*arg));
      *arg = inputNumber;
      printf(" %ld ", inputNumber); 
      tpool_add_work(tm, calculate_square, arg); // add data to the list
    } 
    else if (action == 'w') {     // wait, nothing new happening
      sleep(inputNumber); // the program sleep in num seconds
    } 
    else {
      exit(EXIT_FAILURE);
    }
  }
  fclose(fin);
  tpool_wait(tm); // wait untill all thread finished

  tpool_destroy(tm); //clean the tm structure
  printf("\nResult: %ld %ld %ld %ld\n", sum, odd, min, max);
  return 0;
}
