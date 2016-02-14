#include "molecule/molecule_define.h"

using namespace simula;

simI1 _molecule_::_max_rpos_idx_ = 100;

/** @brief here we land molecule index * 100 + dot indices on substrate */
const simI1 Molecule::land_id(simI1 i) const
{
	simI1 id = 0;
	if (i < size())
	{
		id = self_id() * _molecule_::_max_rpos_idx_ + ridx()[i];
	}
#ifndef NDEBUG
	else
	{
		cout << "ERROR: landing index out of bound" << endl;

	}
#endif
	return id;
}

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
