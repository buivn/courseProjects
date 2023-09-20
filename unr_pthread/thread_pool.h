#ifndef THREAD_POOL_H
#define THREAD_POOL_H

#include <list>
#include <pthread.h>

class Task
{
public:
  Task() {}
  virtual ~Task() {}

  virtual void doit() = 0;
};


class Mutex
{
  pthread_mutex_t id;

public:
  Mutex() {pthread_mutex_init(&id, NULL);}
  ~Mutex() {pthread_mutex_destroy(&id);}

  void lock() {pthread_mutex_lock(&id);}
  void unlock() {pthread_mutex_unlock(&id);}

  pthread_mutex_t* get_id() {return &id;}
};


class Thread_pool
{
  volatile bool state;
  
  std::list<pthread_t> threads;
  
  Mutex mutex;
  
  pthread_cond_t idle, busy;
  
  std::list<Task*> tasks;

public:
  Thread_pool(int size);
  ~Thread_pool();

  void add(Task* task);

private:
  void run();
  static void* runner(void* p);
};

#endif
