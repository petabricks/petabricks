#define USE_MALLOC 0

#include <assert.h>
#include <iostream>

#if defined(_WIN32)
#define _WIN32_WINNT 0x0500
#endif

using namespace std;

#if defined(_WIN32)
#pragma inline_depth(255)
#define inline __forceinline
#define NO_INLINE __declspec(noinline)
#pragma warning(disable: 4530)
#endif

#include "hoard.h"

main()
{
  HoardHeap<128> bunch;

  cout << "size of header = " << sizeof(FooSuperblockHeader<Nada>) << endl;
  cout << "size of bunch = " << sizeof(bunch) << endl;

  const int num = 20431;
  char * buf[num];
#if 0
#if USE_MALLOC
  char * p = (char *) malloc(18000);
#else
  char * p = (char *) bunch.malloc(18000);
#endif
  cout << "p = " << (void *) p << endl;
#if USE_MALLOC
  free (p);
#else
  bunch.free (p);
#endif
#endif

  for (int j = 0; j < 10; j++) {
    for (int i = 0; i < num; i++) {
#if USE_MALLOC
      buf[i] = (char *) malloc(8);
#else
      buf[i] = (char *) bunch.malloc(8);
#endif
    }
  }
  for (int j = 0; j < 10; j++) {
    for (int i = 0; i < num; i++) {
#if USE_MALLOC
      buf[i] = (char*) malloc(8);
#else
      buf[i] = (char *) bunch.malloc (8);
#endif
    }
    for (int i = 0; i < num; i++) {
#if USE_MALLOC
      free (buf[i]);
#else
      bunch.free (buf[i]);
#endif
    }
  }
#if 0
  Supe * s = new Supe (8);
  s->setOwner ((void *) 12);
  s->setPrev (NULL);
  s->setNext (NULL);
  cout << "owner = " << s->getOwner() << endl;
  char * ch;
  ch = (char *) s->malloc (16);
  cout << "s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
  ch = (char *) s->malloc (16);
  cout << "s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
  s->free (ch);
  cout << "free: s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
  ch = (char *) s->malloc (16);
  cout << "s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
  s->clear();
  cout << "clear: s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
  ch = (char *) s->malloc (16);
  cout << "s = " << s << ", ch = " << (void *) ch << ", objects free = " << s->getObjectsFree() << endl;
#endif
}
