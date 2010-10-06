/***************************************************************************
 *  Copyright (C) 2008-2009 Massachusetts Institute of Technology          *
 *                                                                         *
 *  This source code is part of the PetaBricks project and currently only  *
 *  available internally within MIT.  This code may not be distributed     *
 *  outside of MIT. At some point in the future we plan to release this    *
 *  code (most likely GPL) to the public.  For more information, contact:  *
 *  Jason Ansel <jansel@csail.mit.edu>                                     *
 *                                                                         *
 *  A full list of authors may be found in the file AUTHORS.               *
 ***************************************************************************/
#include "petabricksruntime.h"

#include "remotehost.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}

RemoteObjectPtr gen() {
  return new petabricks::RemoteObject();
}


int main(int , const char** ){
  const int port = 2227;
  RemoteHostPtr h;
  RemoteObjectPtr local;

  if(fork()==0){
    h = new RemoteHost();
    h->connect("localhost", port);
  }else{
    h = new RemoteHost();
    jalib::JServerSocket ss(jalib::JSockAddr::ANY, port);
    h->accept(ss);
  }
  jalib::JMutex t;
  t.lock();
  h->createRemoteObject(local=gen(), &gen, &port, sizeof port);
  h->unlockAndRecv(t);
  t.lock();
  h->unlockAndRecv(t);
  JTRACE("end")((intptr_t)local.asPtr());
  return 0;
}

