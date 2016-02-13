#include "molecule.h"

using namespace _molecule_;
using namespace simula;

/** define global variables */
std::vector<Molecule_Type> _molecule_::_rcd_type_;
std::vector<Molecule>      _molecule_::_rcd_list_;

/** @brief generate a new Molecule_Type */
simI1 simula::gen_molecule_type()
{
	// construct a molecule inplace
	_rcd_type_.emplace_back();
	return _rcd_type_.size() - 1;
}

/** @brief generate a new Molecule */
simI1 simula::gen_molecule(const Molecule_Type& type)
{
	// get current molecule index
	simI1 id = _rcd_list_.size();
	// construct a molecule type inplace
	_rcd_list_.emplace_back(&type, id + 1);
	return _rcd_list_.size() - 1;
}
