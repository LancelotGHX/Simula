#include "molecule/molecule_define.h"

using namespace simula;

#ifndef NDEBUG
/** @brief debug function */
void Molecule::debug()
{
  cout << "molecule:" << endl;
  cout << "  index: " << _id_      << endl;
  cout << "  type : " << type_id() << endl;
  cout << " (" << _x_ << "," << _y_ <<")" << endl;
}
#endif
