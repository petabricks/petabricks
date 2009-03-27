#include <string.h>

// Below are generic replacements for the malloc family
// in terms of malloc, free, and malloc_usable_size.

extern "C" void * calloc (size_t nelem, size_t elsize)
{
  size_t n = nelem * elsize;
  void * ptr = malloc (n);
  // Zero out the malloc'd block.
  if (ptr != NULL) {
    memset (ptr, 0, n);
  }
  return ptr;
}


extern "C" char * strndup (const char * s, size_t sz)
{
  char * newString = NULL;
  if (s != NULL) {
#if defined(linux)
    size_t cappedLength = strnlen (s, sz);
#else
    size_t cappedLength = strlen (s);
#endif
    if ((newString = (char *) malloc(cappedLength + 1))) {
      strncpy(newString, s, cappedLength);
      newString[cappedLength] = '\0';
    }
  }
  return newString;
}

extern "C" char * strdup (const char * s)
{
  char * newString = NULL;
  if (s != NULL) {
    if ((newString = (char *) malloc(strlen(s) + 1))) {
      strcpy(newString, s);
    }
  }
  return newString;
}


extern "C" void * realloc (void * ptr, size_t sz)
{
  // realloc acts like malloc when the ptr argument is NULL.
  if (ptr == NULL) {
    ptr = malloc (sz);
    return ptr;
  }
  // and realloc acts like free when the size argument is 0.
  if (sz == 0) {
    free (ptr);
    return NULL;
  }

  // Find out the current size and get a buffer for the new size.
  size_t objSize = malloc_usable_size (ptr);
  void * buf = malloc ((size_t) (sz));

  if (buf != NULL) {
    // Copy the contents of the original object
    // up to the size of the new block.
    size_t minSize = (objSize < sz) ? objSize : sz;
    memcpy (buf, ptr, minSize);
  }

  // Free the old block.
  free(ptr);

  // Return a pointer to the new one.
  return buf;
}


void * operator new (size_t sz)
{
  return malloc (sz);
}

namespace std {
  struct nothrow_t;
}

void * operator new (size_t sz, const std::nothrow_t&) throw() {
  return malloc (sz);
}

void operator delete (void * ptr)
{
  free (ptr);
}

void * operator new[] (size_t sz) {
  return malloc (sz);
}

void * operator new[] (size_t sz, const std::nothrow_t&) throw() {
  return malloc (sz);
}

void operator delete[] (void * ptr)
{
  free (ptr);
}


extern "C" void * memalign (size_t, size_t size)
{
  // NOTE: This function is deprecated and here just acts like malloc.
  return malloc (size);
}

extern "C" void * valloc (size_t sz) {
  return malloc (sz);
}

extern "C" void * pvalloc (size_t sz) {
  return malloc (sz);
}

extern "C" int mallopt (int, int) {
  return 0; // Always fail.
}

