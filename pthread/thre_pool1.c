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
bool done = false;
long inputNumber;
// this mutex for the task function
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;


typedef void (*thread_func_t)(void *arg);

struct tpool_work {
    thread_func_t      func;
    void              *arg;
    struct tpool_work *next;
};
typedef struct tpool_work tpool_work_t;

struct tpool;
struct tpool {
    tpool_work_t    *work_first;
    tpool_work_t    *work_last;
    pthread_mutex_t  work_mutex;
    pthread_cond_t   work_cond;
    pthread_cond_t   working_cond;
    size_t           working_cnt;
    size_t           thread_cnt;
    bool             stop;
};
typedef struct tpool tpool_t;


static tpool_work_t *tpool_work_create(thread_func_t func, void *arg);
// static tpool_work_t *tpool_work_create(void (*func)(void *arg), void *arg);
static tpool_work_t *tpool_work_create(thread_func_t func, void *arg)
{
    tpool_work_t *work;

    if (func == NULL)
        return NULL;

    work       = malloc(sizeof(*work));
    work->func = func;
    work->arg  = arg;
    work->next = NULL;
    return work;
}

static void tpool_work_destroy(tpool_work_t *work);
static void tpool_work_destroy(tpool_work_t *work)
{
    if (work == NULL)
        return;
    free(work);
}

static tpool_work_t *tpool_work_get(tpool_t *tm);
static tpool_work_t *tpool_work_get(tpool_t *tm)
{
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

static void *tpool_worker(void *arg);

static void *tpool_worker(void *arg)
{
    tpool_t      *tm = arg;
    tpool_work_t *work;

    while (1) {
        pthread_mutex_lock(&(tm->work_mutex));

        while (tm->work_first == NULL && !tm->stop)
            pthread_cond_wait(&(tm->work_cond), &(tm->work_mutex));

        if (tm->stop)
            break;

        work = tpool_work_get(tm);
        tm->working_cnt++;
        pthread_mutex_unlock(&(tm->work_mutex));

        if (work != NULL) {
            work->func(work->arg);
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

tpool_t *tpool_create(size_t num);
tpool_t *tpool_create(size_t num)
{
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

    for (i=0; i<num; i++) {
        pthread_create(&thread, NULL, tpool_worker, tm);
        pthread_detach(thread);
    }

    return tm;
}

void tpool_destroy(tpool_t *tm);
void tpool_destroy(tpool_t *tm)
{
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

bool tpool_add_work(tpool_t *tm, thread_func_t func, void *arg);

bool tpool_add_work(tpool_t *tm, thread_func_t func, void *arg)
{
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

void tpool_wait(tpool_t *tm);

void tpool_wait(tpool_t *tm)
{
    if (tm == NULL)
        return;

    pthread_mutex_lock(&(tm->work_mutex));
    while (1) {
        if ((!tm->stop && tm->working_cnt != 0) || (tm->stop && tm->thread_cnt != 0)) {
            pthread_cond_wait(&(tm->working_cond), &(tm->work_mutex));
        }
        else {
            break;
        }
    }
    pthread_mutex_unlock(&(tm->work_mutex));
}

// void worker(void *arg);
// void worker(void *arg)
// {
//     int *val = arg;
//     int  old = *val;

//     *val += 1000;
//     printf("old = %d, val = %d\n", old, *val);

//     if (*val%2)
//         usleep(100000);
// }

// function prototypes
void calculate_square(void *num);

// update global aggregate variables given a number
void calculate_square(void *num)
{
  long number = *((long *) num);
    
  pthread_mutex_lock(&mutex1);
  // printf("Thread %ld: check done condition\n", number);
  // calculate the square
  long the_square = number * number;
  pthread_mutex_unlock(&mutex1);
  sleep(number);
  pthread_mutex_lock(&mutex1);
  // let's add this to our (global) sum
  sum += the_square;
  // now we also tabulate some (meaningless) statistics
  if (number % 2 == 1) {
    // how many of our numbers were odd?
    odd++;
  }
  // what was the smallest one we had to deal with?
  if (number < min) {
    min = number;
  }
  // and what was the biggest one?
  if (number > max) {
    max = number;
  }
  pthread_mutex_unlock(&mutex1);

}


// static const size_t num_threads = 4;
// static const size_t num_items   = 100;

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
  tm   = tpool_create(threadNumber);

  // printf("Check the signal the thread condition 111\n"); 
  char *fn = argv[1];
  // load numbers and add them to the queue
  FILE* fin = fopen(fn, "r");
  char action;
  long inputNumber;
  
  while (fscanf(fin, "%c %ld\n", &action, &inputNumber) == 2) {
    printf("the input nu/mber is: %ld \n", inputNumber);
    if (action == 'p') {            // process, do some work
      long *arg = malloc(sizeof(*arg));
      *arg = inputNumber;
      tpool_add_work(tm, calculate_square, arg);
    } 
    else if (action == 'w') {     // wait, nothing new happening
      // printf("Sleeping at the main function \n");
      sleep(inputNumber); // the program sleep in num seconds
    } 
    else {
      // printf("ERROR: Unrecognized action: '%c'\n", action);
      exit(EXIT_FAILURE);
    }
  }
  fclose(fin);

  // int     *vals;
  // size_t   i; 
  // vals = calloc(num_items, sizeof(*vals));

  
  // for (i=0; i<num_items; i++) {
  //     vals[i] = i;
  //     tpool_add_work(tm, worker, vals+i);
  // }

  tpool_wait(tm);

  // for (i=0; i<num_items; i++) {
  //     printf("%d\n", vals[i]);
  // }
  // print results
  printf("%ld %ld %ld %ld\n", sum, odd, min, max);
  // free(vals);
  // free(&inputNumber);
  tpool_destroy(tm);
  return 0;
}

