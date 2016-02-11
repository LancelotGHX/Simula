#ifndef _SIMULA_SUBSTRATE_
#define _SIMULA_SUBSTRATE_

#include "global.h"
#include "molecule.h"
#include <iostream>
#include <fstream>

#define __BG__ NULL //< background value

using namespace global;

/**
 * Substrate class
 **/
class Substrate {
private:
  const simI1 __xsize__; //< X dimension size
  const simI1 __ysize__; //< Y dimemsion size
  const Molecule*** __sub__;   //< array of pointers to Molecule
private:

  /** @brief allocate the substrate */
  void __alloc__()
  {
    __sub__ = new const Molecule ** [__xsize__];
    for (simI1 i = 0; i < __xsize__; ++i) {
      __sub__[i] = new const Molecule * [__ysize__];
    }
  }

  /** @brief Initialize substrate **/
  void __init__ ()
  {
    for (simI1 i = 0; i < __xsize__; ++i) {
      for (simI1 j = 0; j < __ysize__; ++j) {
	__sub__[i][j] = __BG__;
      }
    }
  }

public:

  /** @brief Point value setter **/
  void set_sub (const simI1 x, const simI1 y, const Molecule * value)
  {
    simI1 mx = pmod(x, __xsize__);
    simI1 my = pmod(y, __ysize__);
    __sub__[mx][my] = value;
  }

  /** @brief Point value getter **/
  const Molecule * get_sub (const simI1 x, const simI1 y) const
  {
    simI1 mx = pmod(x, __xsize__);
    simI1 my = pmod(y, __ysize__);
    return __sub__[mx][my];
  }

  /** @brief Constructor **/
  Substrate (const simI1 Xsize, const simI1 Ysize)
    : __xsize__ (Xsize), 
      __ysize__ (Ysize) 
  {
    //--- initialize dynamic 2D array
    __alloc__();
    //--- initialize array with value 0
    __init__();
  }

  /** @brief Check if the point is empty **/
  simBool is_empty (const simI1 x, const simI1 y) const
  {
    return ( get_sub(x,y) == __BG__ );
  }

  /** @brief Check if the relative positions are all empty **/
  simBool is_all_empty (const simI2* rpos, const simI1 len,
			const simI1 xc, const simI1 yc) const
  {
    simBool empty = true;
    for (simI1 i = 0; i < len; ++i) {
      simI1 x = xc + rpos[i].x;
      simI1 y = yc + rpos[i].y;
      if ( !is_empty(x,y) ) { empty = false; break; }
    }
    return empty;
  }

  /** @brief Land molecule on the position **/
  simBool land (Molecule& m, const simI1 xc, const simI1 yc, const simI1 dc)
  {
    //--- check overlap
    if ( is_all_empty(m.rpos(), m.size(), xc, yc) ) {
      //--- set molecular position & direction
      m.set_x(xc);
      m.set_y(yc);
      m.set_d(dc);
      //--- set point values on substrate
      for (simI1 i = 0; i < m.size(); ++i) {
	simI1 x = xc + m.rpos()[i].x;
	simI1 y = yc + m.rpos()[i].y;
	set_sub(x, y, &m);
      }
      return true;
    } else {
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
  for (simI1 i = 0; i < sub.__xsize__; ++i) {
    for (simI1 j = 0; j < sub.__ysize__; ++j) {
      os << sub.__sub__[i][j] == __BG__ ? 0 : sub.__sub__[i][j]->type_id();
      os << " ";
    }
    os << std::endl;
  }
  return os;
}

#endif // _SIMULA_SUBSTRATE_
