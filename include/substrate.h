#ifndef _SIMULA_SUBSTRATE_
#define _SIMULA_SUBSTRATE_

#include "global.h"
#include "molecule.h"
#include <iostream> // print c++ error
#include <fstream>  // for print substrate to file
#include <vector>   // for constructing substrate

#define __BG__ 0 //< background value

namespace simula 
{

  /**
   * Substrate class
   **/
  class Substrate {
  private:
    const simI1 __xlen__; //< X dimension size
    const simI1 __ylen__; //< Y dimemsion size
    std::vector<simVI1> __sub__; //< array of pointers to Molecule
  private:

    /** @brief Initialize substrate with value background value **/
    void __init__ ()
    {
      for (simI1 i = 0; i < __xlen__; ++i) {
	// construct a column
	simVI1 col;
	for (simI1 j = 0; j < __ylen__; ++j) {
	  col.push_back(__BG__);
	}
	// push the column
	__sub__.push_back(col);
      }
    }

  public:

    /** @brief Constructor **/
    Substrate (const simI1 Xsize, const simI1 Ysize)
      : __xlen__ (Xsize), __ylen__ (Ysize) { __init__(); }

    /** @brief Destructor **/
    ~Substrate () {}

    /** @brief Point value setter **/
    void set_sub (const simI1 x, const simI1 y, const simI1 value)
    {
      simI1 mx = pmod(x, __xlen__);
      simI1 my = pmod(y, __ylen__);
      __sub__[mx][my] = value;
    }

    /** @brief Point value getter **/
    const simI1 get_sub (const simI1 x, const simI1 y) const
    {
      simI1 mx = pmod(x, __xlen__);
      simI1 my = pmod(y, __ylen__);
      return __sub__[mx][my];
    }

    /** @brief Check if the point is empty **/
    simBool is_empty (const simI1 x, const simI1 y) const
    { return ( get_sub(x,y) == __BG__ ); }

    /** @brief Check if the relative positions are all empty **/
    simBool is_empty (const simVI2& rp, const simI1 xc, const simI1 yc) const
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
    simBool land (Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc)
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
    void print(const simChar* name) 
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
    friend std::ostream& operator<<(std::ostream& os, const Substrate& sub);

  };

/** @brief overload output function **/
std::ostream& operator<<(std::ostream& os, const Substrate& sub)
{
  for (simI1 i = 0; i < sub.__xlen__; ++i) {
    for (simI1 j = 0; j < sub.__ylen__; ++j) {
      simI1 v = sub.__sub__[i][j];
      if (v == __BG__) {
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

};


#endif // _SIMULA_SUBSTRATE_
