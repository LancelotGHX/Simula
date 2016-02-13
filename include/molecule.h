#ifndef _SIMULA_MOLECULE_
#define _SIMULA_MOLECULE_

#include "global.h"
#include "molecule/molecule_type_define.h"
#include "molecule/molecule_define.h"

namespace {
  using namespace simula;
  std::vector<Molecule_Type> __rcd_type__;
  std::vector<Molecule>      __rcd_list__;
};

namespace simula 
{

  inline simI1 gen_molecule_type()
  { 
    __rcd_type__.emplace_back();
    return __rcd_type__.size()-1;
  }

  inline Molecule_Type& get_molecule_type(simI1 i)
  {
    return __rcd_type__[i];
  }

  inline simI1 gen_molecule(const Molecule_Type& type)
  {
    simI1 id =  __rcd_list__.size();
    __rcd_list__.emplace_back(&type,id+1);
    return __rcd_list__.size()-1;
  }

  inline Molecule& get_molecule(simI1 i)
  {
    return __rcd_list__[i];
  }
  
  inline const simI1 get_molecule_type_size()
  {
    return __rcd_type__.size();
  }

  inline const simI1 get_molecule_size()
  {
    return __rcd_list__.size();
  } 

  
};

#endif // _SIMULA_MOLECULE_
