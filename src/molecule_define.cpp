#include "molecule/molecule_define.h"

using namespace simula;

#ifndef NDEBUG
/** @brief debug function */
void Molecule::debug()
{
	cout << "molecule:";
	cout << " index: " << _id_;
	cout << " type : " << type_id();
	cout << " (" << _x_ << "," << _y_ << ")" << endl;
}
#endif
