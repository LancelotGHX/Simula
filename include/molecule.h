#ifndef _SIMULA_MOLECULE_
#define _SIMULA_MOLECULE_

#include "global.h"
#include "molecule_type.h"
#include <string>

using namespace std;
using namespace global;

/**
 * Molecule class
 **/
class Molecule {
private:
  //--- unchangable values
  const Molecule_Type* __type__;
  const simI1 __i__; // molecular index
  //--- changable values
  simI1 __d__; // molecular direction
  simI1 __x__; // molecular x-position
  simI1 __y__; // molecular y-position

public:

  /** @brief Constructor **/
  Molecule(const Molecule_Type* tp,
	   const simI1          id) 
    : __type__(tp), __i__(id) {}
  
  /** @brief Destructor **/
  ~Molecule() {}

  /** @brief properties accessor **/
  const Molecule_Type* type() const { return __type__; }
  const simI1  type_id() const { return __type__->id(); }
  const simI1  size() const { return __type__->size(); }
  const simI2* rpos() const { return __type__->rpos(); }
  const simI1  i() const { return __i__; }
  const simI1  x() const { return __x__; }
  const simI1  y() const { return __y__; }
  const simI1  d() const { return __d__; }

  /** @brief properties setters **/
  void set_x(const simI1 xpos) { __x__ = xpos; }
  void set_y(const simI1 ypos) { __y__ = ypos; }
  void set_d(const simI1 direction) { __d__ = direction; }

};

#endif // _SIMULA_MOLECULE_
