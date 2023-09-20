#include <cstdlib>
#include <ctime>
#include <iostream>
#include "thread_pool.h"

using namespace std;

class My_task : public Task
{
  int index;
  Mutex& output;

public:
  My_task(int i, Mutex& out) : index(i), output(out) {}
  ~My_task() {}

  /* Wait for a random number of seconds between 1 and 9,
     then report. Use the mutex to protect the output stream.
  */
  void doit() {
    int s = rand()%9 + 1;
    time_t now = time(NULL);
    while (time(NULL) != (now + (time_t)s));
    output.lock();
    cout << "Task " << index << " took " << s << " seconds." << endl;
    output.unlock();
  }
};

int main(void)
{
  srand(time(NULL));

  /* Mutex to protect the standard output stream (cout).
     The scope of cout is main, so the mutex has to be
     declared in main also.
  */
  Mutex out;
  
  Thread_pool pool(4);

  int i;
  for (i=0; i<10; i++) {
    pool.add(new My_task(i, out));
  }
}
