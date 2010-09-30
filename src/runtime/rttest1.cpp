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

petabricks::PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
petabricks::PetabricksRuntime::Main* petabricksFindTransform(const std::string& name){
  return NULL;
}

petabricks::RemoteObjectPtr gen() {
  return new petabricks::RemoteObject();
}


int main(int argc, const char** argv){
  const int port = 2227;

  jalib::JServerSocket ss(jalib::JSockAddr::ANY, port);

  petabricks::RemoteHost a;

  if(fork()==0){
    a.connect("localhost", port);
  }else{
    a.accept(ss);
  }

  jalib::JMutex t;
  t.lock();
  a.createRemoteObject(gen(), &gen, &port, sizeof port);
  a.unlockAndRecv(t);

  return 0;
}

