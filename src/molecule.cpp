#include "molecule.h"

using namespace _molecule_;
using namespace simula;

/** define global variables */
std::vector<MoleculeType> _molecule_::_rcd_type_;
std::vector<Molecule>      _molecule_::_rcd_list_;

/** @brief generate a new MoleculeType */
simI1 simula::gen_molecule_type()
{
	// construct a molecule inplace
	_rcd_type_.emplace_back();
	_rcd_type_.back().set_gen_id(_rcd_type_.size());
	return _rcd_type_.size() - 1;
}

/** @brief generate a new Molecule */
simI1 simula::gen_molecule(const MoleculeType& type)
{
	// get current molecule index
	simI1 id = _rcd_list_.size();
	// construct a molecule type inplace
	_rcd_list_.emplace_back(&type, id + 1);
	return _rcd_list_.size() - 1;
}
