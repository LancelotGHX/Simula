#include "substrate.h"

#include <ostream> // ostream
#include <fstream> // ofstream

using namespace _substrate_;
using namespace simula;

const simI1 _substrate_::_bg_ = 0; //< background value
Substrate * _substrate_::_sub_ = NULL;

/** @brief Initialize substrate with value background value **/
void Substrate::_init_()
{
	_data_ = new simI1[_xlen_ * _ylen_];
	for (simI1 i = 0; i < _xlen_ * _ylen_; ++i)
	{
		_data_[i] = _bg_;
	}
}

/** @brief Point value getter **/
const simI1 Substrate::get_sub(const simI1 x, const simI1 y) const
{
	simI1 mx = pmod(x, _xlen_);
	simI1 my = pmod(y, _ylen_);
	return _data_[my * _xlen_ + mx];
}

/** @brief Point value setter **/
void Substrate::set_sub(const simI1 x, const simI1 y, const simI1 value)
{
	simI1 mx = pmod(x, _xlen_);
	simI1 my = pmod(y, _ylen_);
	_data_[my * _xlen_ + mx] = value;
}

/** @brief Check if the relative positions are all empty **/
simBool Substrate::is_empty
(const simVI2& rp, const simI1 xc, const simI1 yc) const
{
	simBool empty = true;
	for (simI1 i = 0; i < rp.size(); ++i) {
		simI1 x = xc + rp[i].x;
		simI1 y = yc + rp[i].y;
		if (!is_empty(x, y)) { empty = false; break; }
	}
	return empty;
}

/** @brief Land molecule on the position **/
simBool Substrate::land
(Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc)
{
	//--- check overlap
	if (is_empty(m.rpos(), xc, yc))
	{
		//--- set molecular position & direction
		m.set_x(xc);
		m.set_y(yc);
		m.set_d(dc);
		//--- set point values on substrate
		for (simI1 i = 0; i < m.size(); ++i) {
			simI1 x = xc + m.rpos()[i].x;
			simI1 y = yc + m.rpos()[i].y;
			set_sub(x, y, m.land_id(i) /* land index containing molecule index and rpos index */);
		}
		return true;
	}
	else {
#ifndef NDEBUG
		cout << "landing fail" << endl;
#endif
		//--- make no changes for checking failure
		return false;
	}
}

/** @brief print substrate into file **/
void Substrate::print(const simChar* name)
{
	std::ofstream file(name);
	if (file.is_open()) 
	{
		file << *this;
		file.close();
	}
	else {
#ifndef NDEBUG
		std::cerr << "fail to open a file" << std::endl;
#endif
	}
}

/** @brief overload output function **/
std::ostream& simula::operator<<(std::ostream& os, const Substrate& sub)
{
	for (simI1 i = 0; i < sub._xlen_; ++i) {
		for (simI1 j = 0; j < sub._ylen_; ++j) {
			// check if the point is occupied by something
			simI1 v = lid2sid(sub.get_sub(i, j));
			// if there is nothing, filled by zero
			if (v == _bg_) {
				os << 0 << " ";
			}
			// if there is something, fill its type index instead the self_id (reference index/data index)
			else {
				// since all indices are counted from 1, we need to substract 1 first
				os << get_molecule(v - 1).type_id() << " ";
			}
		}
		os << std::endl;
	}
	return os;
}

void simula::gen_sub(simI1 xlen, simI1 ylen)
{
	if (!_substrate_::_sub_) 
	{
#ifndef NDEBUG
		cout << " **** generating substrate ****\n\n";
#endif
		_substrate_::_sub_ = new Substrate(xlen, ylen);
	}
	else {
#ifndef NDEBUG
		cout << " ERROR: substrate is already generated\n\n";
#endif
	}
}
