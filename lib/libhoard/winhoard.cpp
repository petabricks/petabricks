/* -*- C++ -*- */

/*
  The Hoard Multiprocessor Memory Allocator
  www.hoard.org

  Author: Emery Berger, http://www.cs.umass.edu/~emery
 
  Copyright (c) 1998-2005, The University of Texas at Austin

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

/*

  Compile with compile-winhoard.cmd.

 */


#include <windows.h>

#define WIN32_LEAN_AND_MEAN
#define _WIN32_WINNT 0x0500

#pragma inline_depth(255)

#pragma warning(disable: 4273)
#pragma warning(disable: 4098)  // Library conflict.
#pragma warning(disable: 4355)  // 'this' used in base member initializer list.
#pragma warning(disable: 4074)	// initializers put in compiler reserved area.

//#pragma init_seg(compiler)
#pragma comment(linker, "/merge:.CRT=.data")
#pragma comment(linker, "/disallowlib:libc.lib")
#pragma comment(linker, "/disallowlib:libcd.lib")
#pragma comment(linker, "/disallowlib:libcmt.lib")
#pragma comment(linker, "/disallowlib:libcmtd.lib")
#pragma comment(linker, "/disallowlib:msvcrtd.lib")

void (*hoard_memcpy_ptr)(void *dest, const void *src, size_t count);
void (*hoard_memset_ptr)(void *dest, int c, size_t count);

const char *RlsCRTLibraryName[] = {"MSVCR71.DLL", "MSVCR80.DLL"};
const int RlsCRTLibraryNameLength = 2;
const char *DbgCRTLibraryName = "MSVCRTD.DLL";

#define IAX86_NEARJMP_OPCODE	  0xe9
#define MakeIAX86Offset(to,from)  ((unsigned)((char*)(to)-(char*)(from)) - 5)

typedef struct
{
  const char *import;		// import name of patch routine
  FARPROC replacement;		// pointer to replacement function
  FARPROC original;		// pointer to original function
  unsigned char codebytes[5];	// 5 bytes of original code storage
} PATCH;


static bool PatchMeIn (void);

#define CUSTOM_PREFIX(n) hoard_##n
#define HOARD_PRE_ACTION {DisableThreadLibraryCalls ((HMODULE)hinstDLL); PatchMeIn();}
#define HOARD_POST_ACTION {HeapAlloc (GetProcessHeap(), 0, 1); }

#define CUSTOM_DLLNAME HoardDllMain

#include "libhoard.cpp"

// Intercept the exit functions.

static const int HOARD_MAX_EXIT_FUNCTIONS = 255;
static int exitCount = 0;

extern "C" {

  __declspec(dllexport) int ReferenceHoardStub;

  typedef void (*exitFunctionType) (void);
  exitFunctionType exitFunctionBuffer[255];

  void hoard_onexit (void (*function)(void)) {
    if (exitCount < HOARD_MAX_EXIT_FUNCTIONS) {
      exitFunctionBuffer[exitCount] = function;
      exitCount++;
    }
  }

  void hoard_exit (int code) {
    while (exitCount > 0) {
      exitCount--;
      (exitFunctionBuffer[exitCount])();
    }
  }

  void * hoard_expand (void * ptr) {
    return NULL;
  }

}


/* ------------------------------------------------------------------------ */

static PATCH rls_patches[] = 
  {
    // RELEASE CRT library routines supported by this memory manager.

#if 0
    {"_heapchk",	(FARPROC) hoard__heapchk,	0},
    {"_heapmin",	(FARPROC) hoard__heapmin,	0},
    {"_heapset",	(FARPROC) hoard__heapset,	0},
    {"_heapwalk",	(FARPROC) hoard__heapwalk,	0},
#endif

    {"_expand",		(FARPROC) hoard_expand,	   0},
    {"_onexit",         (FARPROC) hoard_onexit,    0},
    {"_exit",           (FARPROC) hoard_exit,      0},

    // operator new, new[], delete, delete[].

    {"??2@YAPAXI@Z",    (FARPROC) hoard_malloc,    0},
    {"??_U@YAPAXI@Z",   (FARPROC) hoard_malloc,    0},
    {"??3@YAXPAX@Z",    (FARPROC) hoard_free,      0},
    {"??_V@YAXPAX@Z",   (FARPROC) hoard_free,      0},

    // the nothrow variants new, new[].

    {"??2@YAPAXIABUnothrow_t@std@@@Z",  (FARPROC) hoard_malloc, 0},
    {"??_U@YAPAXIABUnothrow_t@std@@@Z", (FARPROC) hoard_malloc, 0},

    {"_msize",	(FARPROC) hoard_malloc_usable_size,		0},
    {"calloc",	(FARPROC) hoard_calloc,		0},
    {"malloc",	(FARPROC) hoard_malloc,		0},
    {"realloc",	(FARPROC) hoard_realloc,		0},
    {"free",	(FARPROC) hoard_free,              0},
  };

#ifdef _DEBUG
static PATCH dbg_patches[] = 
  {
    // DEBUG CRT library routines supported by this memory manager.

    {"_calloc_dbg",               (FARPROC) hoard__calloc_dbg,0},
    {"_CrtCheckMemory",	          (FARPROC) hoard__CrtCheckMemory,	0},
    {"_CrtDoForAllClientObjects", (FARPROC) hoard__CrtDoForAllClientObjects, 0},
    {"_CrtDumpMemoryLeaks",       (FARPROC) hoard__CrtDumpMemoryLeaks, 0},
    {"_CrtIsMemoryBlock",         (FARPROC) hoard__CrtIsMemoryBlock, 0},
    {"_CrtIsValidHeapPointer",	  (FARPROC) hoard__CrtIsValidHeapPointer, 0},
    {"_CrtMemCheckpoint",         (FARPROC) hoard__CrtMemCheckpoint, 0},
    {"_CrtMemDifference",         (FARPROC) hoard__CrtMemDifference, 0},
    {"_CrtMemDumpAllObjectsSince",(FARPROC) hoard__CrtMemDumpAllObjectsSince, 0},
    {"_CrtMemDumpStatistics",	  (FARPROC) hoard__CrtMemDumpStatistics, 0},
    {"_CrtSetAllocHook",	  (FARPROC) hoard__CrtSetAllocHook, 0},
    {"_CrtSetBreakAlloc",         (FARPROC) hoard__CrtSetBreakAlloc,0},
    {"_CrtSetDbgFlag",	          (FARPROC) hoard__CrtSetDbgFlag, 0},
    {"_CrtSetDumpClient",(FARPROC) hoard__CrtSetDumpClient, 0},
    {"_expand",		 (FARPROC) hoard__expand, 0},
    {"_expand_dbg",      (FARPROC) hoard__expand_dbg, 0},
    {"_free_dbg",	 (FARPROC) hoard__free_dbg, 0},
    {"_malloc_dbg",      (FARPROC) hoard__malloc_dbg, 0},
    {"_msize",		 (FARPROC) hoard__msize, 0},
    {"_msize_dbg",	 (FARPROC) hoard__msize_dbg, 0},
    {"_realloc_dbg",     (FARPROC) hoard__realloc_dbg, 0},
    {"_heapchk",	 (FARPROC) hoard__heapchk,	0},
    {"_heapmin",	 (FARPROC) hoard__heapmin,	0},
    {"_heapset",	 (FARPROC) hoard__heapset,	0},
    {"_heapwalk",	 (FARPROC) hoard__heapwalk, 0},
    {"_msize",		 (FARPROC) hoard__msize, 0},
    {"calloc",		 (FARPROC) hoard_calloc, 0},
    {"malloc",		 (FARPROC) hoard_malloc, 0},
    {"realloc",		 (FARPROC) hoard_realloc, 0},
    {"free",             (FARPROC) hoard_free, 0},

    // operator new, new[], delete, delete[].

    {"??2@YAPAXI@Z",     (FARPROC) hoard_malloc, 0},
    {"??_U@YAPAXI@Z",    (FARPROC) hoard_malloc, 0},
    {"??3@YAXPAX@Z",     (FARPROC) hoard_free,   0},
    {"??_V@YAXPAX@Z",    (FARPROC) hoard_free,   0},

    // the nothrow variants new, new[].

    {"??2@YAPAXIABUnothrow_t@std@@@Z",  (FARPROC) hoard_new_nothrow, 0},
    {"??_U@YAPAXIABUnothrow_t@std@@@Z", (FARPROC) hoard_new_nothrow, 0},

    // The debug versions of operator new & delete.

    {"??2@YAPAXIHPBDH@Z", (FARPROC) hoard_debug_operator_new, 0},
    {"??3@YAXPAXHPBDH@Z", (FARPROC) hoard_debug_operator_delete, 0},
    // And the nh_malloc_foo.

    {"_nh_malloc_dbg",   (FARPROC)hoard_nh_malloc_dbg, 0},
  };
#endif


static void PatchIt (PATCH *patch)
{
  // Change rights on CRT Library module to execute/read/write.

  MEMORY_BASIC_INFORMATION mbi_thunk;
  VirtualQuery((void*)patch->original, &mbi_thunk, 
	       sizeof(MEMORY_BASIC_INFORMATION));
  VirtualProtect(mbi_thunk.BaseAddress, mbi_thunk.RegionSize, 
		 PAGE_EXECUTE_READWRITE, &mbi_thunk.Protect);

  // Patch CRT library original routine:
  // 	save original 5 code bytes for exit restoration
  //		write jmp <patch_routine> (5 bytes long) to original.

  memcpy(patch->codebytes, patch->original, sizeof(patch->codebytes));
  unsigned char *patchloc = (unsigned char*)patch->original;
  *patchloc++ = IAX86_NEARJMP_OPCODE;
  *(unsigned*)patchloc = MakeIAX86Offset(patch->replacement, patch->original);
	
  // Reset CRT library code to original page protection.

  VirtualProtect(mbi_thunk.BaseAddress, mbi_thunk.RegionSize, 
		 mbi_thunk.Protect, &mbi_thunk.Protect);
}


static bool PatchMeIn (void)
{
  // acquire the module handles for the CRT libraries (release and debug)
  for (int i = 0; i < RlsCRTLibraryNameLength; i++) {

    HMODULE RlsCRTLibrary = GetModuleHandle(RlsCRTLibraryName[i]);

#ifdef _DEBUG
    HMODULE DbgCRTLibrary = GetModuleHandle(DbgCRTLibraryName);
#endif
    
    HMODULE DefCRTLibrary = 
#ifdef _DEBUG
      DbgCRTLibrary? DbgCRTLibrary: 
#endif	
      RlsCRTLibrary;

    // assign function pointers for required CRT support functions
    if (DefCRTLibrary) {
      hoard_memcpy_ptr = (void(*)(void*,const void*,size_t))
	GetProcAddress(DefCRTLibrary, "memcpy");
      hoard_memset_ptr = (void(*)(void*,int,size_t))
	GetProcAddress(DefCRTLibrary, "memset");
    }

    // patch all relevant Release CRT Library entry points
    bool patchedRls = false;
    if (RlsCRTLibrary) {
      for (int i = 0; i < sizeof(rls_patches) / sizeof(*rls_patches); i++) {
	if (rls_patches[i].original = GetProcAddress(RlsCRTLibrary, rls_patches[i].import)) {
	  PatchIt(&rls_patches[i]);
	  patchedRls = true;
	}
      }
    }

#ifdef _DEBUG
    // patch all relevant Debug CRT Library entry points
    bool patchedDbg = false;
    if (DbgCRTLibrary) {
      for (int i = 0; i < sizeof(dbg_patches) / sizeof(*dbg_patches); i++) {
	if (dbg_patches[i].original = GetProcAddress(DbgCRTLibrary, dbg_patches[i].import)) {
	  PatchIt(&dbg_patches[i]);
	  patchedDbg = true;
	}
      }
    }
#endif
  }
  return true;
}
