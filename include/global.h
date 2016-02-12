#ifndef _SIMULA_GLOBAL_
#define _SIMULA_GLOBAL_

#include "global/global_type.h"
#include "global/global_function.h"

#ifndef NDEBUG
#include <iostream>
using namespace std;
#endif

///////////////////////////////////////////////////////////////////////////////
// constant definitions
//
namespace simula
{

  /** @brief global variables */
  const simI1 subYsize = 50;
  const simI1 subXsize = 50;
  simI1 _list_[10] = { 0,0,1,0,0,1,-1,0,0,-1 };

};

#endif // _SIMULA_GLOBAL_
