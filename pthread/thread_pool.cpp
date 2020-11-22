#include "thread_pool.h"

using namespace std;

Thread_pool::Thread_pool(int size) : state(true)
{
  pthread_cond_init(&idle, NULL);
  pthread_cond_init(&busy, NULL);

  int i;
  for (i=0; i<size; i++) {
    pthread_t t;
    pthread_create(&t, NULL, runner, this);
    threads.push_back(t);
  }
}

Thread_pool::~Thread_pool()
{
  mutex.lock();
  state = false;
  mutex.unlock();

  pthread_cond_broadcast(&idle);
  
  list<pthread_t>::iterator it;
  for (it=threads.begin(); it!=threads.end(); ++it) {
    pthread_join(*it, NULL);
  }
  
  pthread_cond_destroy(&idle);
  pthread_cond_destroy(&busy);
}

void Thread_pool::add(Task* task)
{
  mutex.lock();

  while (tasks.size() >= threads.size()) {
    pthread_cond_wait(&busy, mutex.get_id());
  }

  tasks.push_back(task);

  pthread_cond_signal(&idle);

  mutex.unlock();
}

void Thread_pool::run()
{
  while (true) {

    mutex.lock();

    while ((state==true) && tasks.empty()) {
      pthread_cond_wait(&idle, mutex.get_id());
    }

    if ((state==false) && tasks.empty()) {
      mutex.unlock();
      pthread_exit(NULL);
    }

    Task *task = tasks.front();
    tasks.pop_front();

    pthread_cond_signal(&busy);

    mutex.unlock();

    task->doit();

    delete task;
  }
}

void* Thread_pool::runner(void* p)
{
  ((Thread_pool*)p)->run();
  return NULL;
}
