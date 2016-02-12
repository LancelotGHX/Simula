#ifndef _SIMULA_MOLECULE_
#define _SIMULA_MOLECULE_

#include "global.h"
#include "molecule/molecule_type_define.h"
#include "molecule/molecule_define.h"
#include <vector>

namespace {
  using namespace simula;
  std::vector<Molecule_Type> __rcd_type__;
  std::vector<Molecule>      __rcd_list__;
};

namespace simula 
{

  simI1 gen_molecule_type()
  { 
    __rcd_type__.emplace_back();
    return __rcd_type__.size()-1;
  }

  Molecule_Type& get_molecule_type(simI1 i)
  {
    return __rcd_type__[i];
  }

  simI1 gen_molecule(const Molecule_Type& type)
  {
    simI1 id =  __rcd_list__.size();
    __rcd_list__.emplace_back(&type,id+1);
    return __rcd_list__.size()-1;
  }

  Molecule& get_molecule(simI1 i)
  {
    return __rcd_list__[i];
  }
  
  const simI1 get_molecule_type_size()
  {
    return __rcd_type__.size();
  }

  const simI1 get_molecule_size()
  {
    return __rcd_list__.size();
  } 

  
};

#endif // _SIMULA_MOLECULE_
