/***************************************************************************
 *   Copyright (C) 2008 by Jason Ansel                                     *
 *   jansel@csail.mit.edu                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include "jthread.h"
#include "jassert.h"
#include <signal.h>
#include <ucontext.h>

// #ifdef __x86_64__
// # define eax rax
// # define ebx rbx
// # define ecx rcx
// # define edx rax
// # define ebp rbp
// # define esi rsi
// # define edi rdi
// # define esp rsp
// # define CLEAN_FOR_64_BIT(args...) CLEAN_FOR_64_BIT_HELPER(args)
// # define CLEAN_FOR_64_BIT_HELPER(args...) #args
// #else
// # define CLEAN_FOR_64_BIT(args...) #args
// #endif
// 
// 
// static void _switchToNewStack(char* stack, void (*fn)()){
//   asm volatile (CLEAN_FOR_64_BIT(mov %0,%%esp\n\t)
//                 CLEAN_FOR_64_BIT(xor %%ebp,%%ebp\n\t) 
//                 :: "g" (stack) : "memory");
//   (*fn)();
//   asm volatile ("hlt");
// }

static __thread jalib::JThread*        curThread = 0;
static __thread ucontext_t             idleContext;
namespace jalib {
  
class JThreadContext {
  static void theUserThreadStart(){
    curThread->run();
    JASSERT(curThread!=0);
    curThread->_state = JThread::STOPPED;
    JASSERT(setcontext(&idleContext)==0);
  }
  public:
    void startRunning(){
      JASSERT(curThread!=0);
      curThread->_state = JThread::RUNNING;
      JASSERT(getcontext(&context)==0);
      context.uc_stack.ss_sp = stack;
      context.uc_stack.ss_size = sizeof stack;
      makecontext(&context, (void (*)())&theUserThreadStart, 0);
      JASSERT(setcontext(&context)==0);
    }
    void continueRunning(){
      JASSERT(curThread!=0);
      curThread->_state = JThread::RUNNING;
      JASSERT(setcontext(&context)==0);
      JASSERT(false);
    }
    void suspendRunning(){
      JASSERT(curThread!=0);
      curThread->_state = JThread::SUSPENDED;
      JASSERT(getcontext(&context)==0);
      JASSERT(setcontext(&idleContext)==0);
    }
  protected:
    char stack[8192];
    ucontext_t context;
};

}


jalib::JThread::JThread() : _stack(NULL), _state(STOPPED){
}

jalib::JThread::~JThread(){
  JASSERT(_state == STOPPED).Text("Can't destruct a running thread.");
}

void jalib::JThread::start(){
  JASSERT(_state == STOPPED);
  pendingThreadPool().push(this);
}

void jalib::JThread::yield(){
  JASSERT(_state == RUNNING);
  _stack->suspendRunning();
}

jalib::JThreadQueue& jalib::JThread::pendingThreadPool(){
  static JThreadQueue inst;//singleton
  return inst;
}




void* jalib::JThread::systemThreadMain(void*){
  JASSERT(getcontext(&idleContext)==0);
  
  if(curThread!=NULL){
    if(curThread->_state == SUSPENDED){
      JNOTE("suspended thread");
      pendingThreadPool().push(curThread);
    }
    if(curThread->_state == STOPPED){
      JNOTE("stopped thread");
      delete curThread->_stack;
      curThread->_stack = NULL;
    }
  }

  curThread = pendingThreadPool().pop();
  switch(curThread->_state){
    case STOPPED:
      JNOTE("start");
      curThread->_stack = new JThreadContext();
      curThread->_stack->startRunning();
      break;
    case SUSPENDED:
      JNOTE("continue");
      curThread->_stack->continueRunning();
      break;
    default:
      JASSERT(false);
  }

  return NULL;
}

void jalib::JThread::launchThreads(){
  pthread_t t;
  pthread_create(&t, NULL, systemThreadMain, NULL);
  systemThreadMain(NULL);
}
