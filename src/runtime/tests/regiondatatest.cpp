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
#include "petabricks.h"

#include "regiondataraw.h"
#include "regiondatasplit.h"

using namespace petabricks;

PetabricksRuntime::Main* petabricksMainTransform(){
  return NULL;
}
PetabricksRuntime::Main* petabricksFindTransform(const std::string& ){
  return NULL;
}
void _petabricksInit() {}
void _petabricksCleanup() {}

int main(int /*argc*/, const char** /*argv*/){
  //const char* filename1 = "testdata/Helmholtz3DZeros";
  const char* filename2 = "testdata/Helmholtz3DB1";

  IndexT m0[] = {0,0,0};
  //IndexT m1[] = {1,1,1};
  //IndexT m123[] = {1,2,3};
  //IndexT m2[] = {2,2,2};
  IndexT m3[] = {3,3,3};

  RegionDataRawPtr regiondata = new RegionDataRaw(filename2);

  regiondata->print();

  printf("before %4.8g\n", regiondata->readCell(m0));
  regiondata->writeCell(m0, 5);
  printf("after %4.8g\n", regiondata->readCell(m0));

  RegionDataSplitPtr regionDataSplit = new RegionDataSplit(regiondata, m3);
  regionDataSplit->print();

  printf("completed\n");
}
