#include "molecule/molecule_type_define.h"

using namespace simula;

/** @brief set type name **/
void MoleculeType::set_name(const simString& name)
{
	_name_ = name;
}

/** @brief set type index **/
void MoleculeType::set_gen_id(const simI1 id)
{
	_gen_idx_ = id;
}

/** @brief set type index **/
void MoleculeType::set_usr_id(const simI1 id)
{
	_usr_idx_ = id;
}

/** @brief set type amount **/
void MoleculeType::set_amount(const simI1 number)
{
	_amount_ = number;
}

/** @brief set relative component position with a list of integer pairs **/
void MoleculeType::set_rpos(const simVI2& rpos, const simVI1& ridx)
{
	_dot_pos_ = rpos; 
	_dot_idx_ = ridx;
}
void MoleculeType::set_rpos(const simI1 sz, const simI1* ls)
{
	for (simI1 i = 0; i < sz; ++i) 
	{
		simI1 xid = 2 * i;
		simI1 yid = 2 * i + 1;
		_dot_pos_.push_back(simI2(ls[xid], ls[yid]));
	}
}

void MoleculeType::set_bond(const std::vector<simOneBond>& bonds)
{
	_bond_ = bonds;
}

#ifndef NDEBUG
/** @brief  properties **/
void MoleculeType::debug()
{
	cout << "==> Molecule" << endl;
	cout << " ** name: " << _name_ << endl;
	cout << " ** amount: " << _amount_ << endl;
	cout << " ** gen index: " << _gen_idx_ << endl;
	cout << " ** usr index: " << _usr_idx_ << endl;
	for (simI1 i = 0; i < this->size(); ++i) {
		cout << " ** pos id = " << _dot_idx_[i] << " (";
		cout << _dot_pos_[i].x << ",";
		cout << _dot_pos_[i].y << ")\n";
	}
	cout << endl;

	if (_bond_.size() != 0) {
		for (simI1 i = 0; i < _bond_.size(); ++i) {
			cout << " ==> bond information:";
			cout << "\n    => number " << _bond_[i].rpos.size();
			cout << "\n    => energy " << _bond_[i].energy;
			cout << "\n    => target " << _bond_[i].target;
			for (simI1 j = 0; j < _bond_[i].rpos.size(); ++j) {
				cout << "\n    => rpos (id,rx,ry) = (" << _bond_[i].rpos[j].z;
				cout << "," << _bond_[i].rpos[j].x;
				cout << "," << _bond_[i].rpos[j].y;
				cout << ")";
			}
			cout << "\n\n";
		}
	}
	else {
		cout << " no bound !!!!!\n\n" << endl;
	}

}
#endif
