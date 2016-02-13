#include "substrate.h"

using namespace _substrate_;
using namespace simula;

const simI1 _substrate_::_bg_ = 0; //< background value

/** @brief Initialize substrate with value background value **/
void Substrate::_init_ ()
{
  for (simI1 i = 0; i < _xlen_; ++i) {
    // construct a column
    simVI1 col;    
    for (simI1 j = 0; j < _ylen_; ++j)
      col.push_back(_bg_);
    // push the column
    _sub_.push_back(col);
  }
}

/** @brief Point value setter **/
void Substrate::set_sub (const simI1 x, const simI1 y, const simI1 value)
{
  simI1 mx = pmod(x, _xlen_);
  simI1 my = pmod(y, _ylen_);
  _sub_[mx][my] = value;
}

/** @brief Point value getter **/
const simI1 Substrate::get_sub (const simI1 x, const simI1 y) const
{
  simI1 mx = pmod(x, _xlen_);
  simI1 my = pmod(y, _ylen_);
  return _sub_[mx][my];
}

/** @brief Check if the relative positions are all empty **/
simBool Substrate::is_empty
(const simVI2& rp, const simI1 xc, const simI1 yc) const
{
  simBool empty = true;
  for (simI1 i = 0; i < rp.size(); ++i) {
    simI1 x = xc + rp[i].x;
    simI1 y = yc + rp[i].y;
    if ( !is_empty(x,y) ) { empty = false; break; }
  }
  return empty;
}

/** @brief Land molecule on the position **/
simBool Substrate::land
(Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc)
{
  //--- check overlap
  if ( is_empty(m.rpos(), xc, yc) ) {
    //--- set molecular position & direction
    m.set_x(xc);
    m.set_y(yc);
    m.set_d(dc);
    //--- set point values on substrate
    for (simI1 i = 0; i < m.size(); ++i) {
      simI1 x = xc + m.rpos()[i].x;
      simI1 y = yc + m.rpos()[i].y;
      set_sub(x, y, m.self_id());
    }
    return true;
  } else {
#ifndef NDEBUG
    cout << "landing fail"<<endl;
#endif
    //--- make no changes for checking failure
    return false;
  }
}

/** @brief print substrate into file **/
void Substrate::print(const simChar* name) 
{
  std::ofstream file(name);
  if (file.is_open()) {
    file << *this;
    file.close();
  } else {
    std::cerr << "fail to open a file" << std::endl;
  }
}

/** @brief overload output function **/
std::ostream& simula::operator<<(std::ostream& os, const Substrate& sub)
{
  for (simI1 i = 0; i < sub._xlen_; ++i) {
    for (simI1 j = 0; j < sub._ylen_; ++j) {
      simI1 v = sub._sub_[i][j];
      if (v == _bg_) {
	os << 0 << " ";
      } else {
	// since all indices are counted from 1, we need to substract 1 first
	os << get_molecule(v-1).type_id() << " ";	
      }
    }
    os << std::endl;
  }
  return os;
}
