#ifndef _SIMULA_MOLECULE_
#define _SIMULA_MOLECULE_

#include "molecule/molecule_define.h"

namespace _molecule_ {
  extern std::vector<simula::Molecule_Type> _rcd_type_;
  extern std::vector<simula::Molecule>      _rcd_list_;
};

namespace simula {

  /** @brief generate a new Molecule_Type */
  simI1 gen_molecule_type ();
  /** @brief generate a new Molecule */
  simI1 gen_molecule (const Molecule_Type& type);
  /** @brief get the Molecule_Type by index */
  inline Molecule_Type& get_molecule_type (simI1 i)
  {
    return _molecule_::_rcd_type_[i];
  }
  /** @brief get the Molecule by index */
  inline Molecule& get_molecule (simI1 i)
  {
    return _molecule_::_rcd_list_[i];
  }
  /** @brief get the number of Molecule_Type */
  inline const simI1 get_molecule_type_size ()
  {
    return _molecule_::_rcd_type_.size();
  }
  /** @brief get the number of Molecule */
  inline const simI1 get_molecule_size ()
  {
    return _molecule_::_rcd_list_.size();
  } 

};

#endif // _SIMULA_MOLECULE_
