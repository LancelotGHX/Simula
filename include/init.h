#ifndef _SIMULA_INIT_
#define _SIMULA_INIT_

#include "molecule.h"

namespace {
  using namespace simula;
  void __rcd_init__(const Molecule_Type& tp) 
  {
    for (simI1 id = 0; id < tp.amount(); ++id) {
      rcd.push_back(Molecule(&tp, id));
    }
  }

};

namespace simula
{

  void init(const Molecule_Type& tp) 
  {
    __rcd_init__(tp);
  }

};


#endif // _SIMULA_INIT_
